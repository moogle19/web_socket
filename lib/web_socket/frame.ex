defmodule WebSocket.Frame do
  @moduledoc false

  # Functions and data structures for describing websocket frames.
  # https://tools.ietf.org/html/rfc6455#section-5.2

  import Record
  alias WebSocket.{Utils, Extension}
  alias WebSocket.Error

  @typedoc """
  Shorthand notations for control frames

  * `:ping` - shorthand for `{:ping, ""}`
  * `:pong` - shorthand for `{:pong, ""}`
  * `:close` - shorthand for `{:close, nil, nil}`

  These may be passed to `encode/2`. Frames decoded with `decode/2` are always
  in `t:frame/0` format.
  """
  @type shorthand_frame :: :ping | :pong | :close

  @typedoc """
  A WebSocket frame

  * `{:binary, binary}` - a frame containing binary data. Binary frames
    can be used to send arbitrary binary data such as a PDF.
  * `{:text, text}` - a frame containing string data. Text frames must be
    valid utf8. Elixir has wonderful support for utf8: `String.valid?/1`
    can detect valid and invalid utf8.
  * `{:ping, binary}` - a control frame which the server should respond to
    with a pong. The binary data must be echoed in the pong response.
  * `{:pong, binary}` - a control frame which forms a reply to a ping frame.
    Pings and pongs may be used to check the a connection is alive or to
    estimate latency.
  * `{:close, code, reason}` - a control frame used to request that a connection
    be closed or to acknowledgee a close frame send by the server.

  These may be passed to `encode/2` or returned from `decode/2`.

  ## Close frames

  In order to close a WebSocket connection gracefully, either the client or
  server sends a close frame. Then the other endpoint responds with a
  close with code `1_000` and then closes the TCP connection. This can be
  accomplished in WebSocket like so:

  ```elixir
  {:ok, websocket, data} = WebSocket.encode(websocket, :close)
  {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

  close_response = receive(do: (message -> message))
  {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, close_response)
  {:ok, websocket, [{:close, 1_000, ""}]} = WebSocket.decode(websocket, data)

  HTTP.close(conn)
  ```

  [rfc6455
  section 7.4.1](https://datatracker.ietf.org/doc/html/rfc6455#section-7.4.1)
  documents codes which may be used in the `code` element.
  """
  @type frame ::
          {:text, String.t()}
          | {:binary, binary()}
          | {:ping, binary()}
          | {:pong, binary()}
          | {:close, code :: non_neg_integer() | nil, reason :: binary() | nil}

  @compile {:inline, apply_mask: 2, apply_mask: 3}

  shared = [{:reserved, <<0::size(3)>>}, :mask, :data, :fin?]

  defrecord :continuation, shared
  defrecord :text, shared
  defrecord :binary, shared
  # > All control frames MUST have a payload length of 125 bytes or less
  # > and MUST NOT be fragmented.
  defrecord :close, shared ++ [:code, :reason]
  defrecord :ping, shared
  defrecord :pong, shared

  defguard is_control(frame)
           when is_tuple(frame) and
                  (elem(frame, 0) == :close or elem(frame, 0) == :ping or elem(frame, 0) == :pong)

  defguard is_fin(frame) when elem(frame, 4) == true

  # guards frames dealt with in the user-space (not records)
  defguardp is_friendly_frame(frame)
            when frame in [:ping, :pong, :close] or
                   (is_tuple(frame) and elem(frame, 0) in [:text, :binary, :ping, :pong] and
                      is_binary(elem(frame, 1))) or
                   (is_tuple(frame) and elem(frame, 0) == :close and is_integer(elem(frame, 1)) and
                      is_binary(elem(frame, 2)))

  # https://tools.ietf.org/html/rfc6455#section-7.4.1
  @invalid_status_codes [1_004, 1_005, 1_006, 1_016, 1_100, 2_000, 2_999]
  # https://tools.ietf.org/html/rfc6455#section-7.4.2
  defguardp is_valid_close_code(code)
            when code in 1_000..4_999 and code not in @invalid_status_codes

  @opcodes %{
    # non-control opcodes:
    continuation: <<0x0::size(4)>>,
    text: <<0x1::size(4)>>,
    binary: <<0x2::size(4)>>,
    # 0x3-7 reserved for future non-control frames
    # control opcodes:
    close: <<0x8::size(4)>>,
    ping: <<0x9::size(4)>>,
    pong: <<0xA::size(4)>>
    # 0xB-F reserved for future control frames
  }
  @reverse_opcodes Map.new(@opcodes, fn {k, v} -> {v, k} end)
  @non_control_opcodes [:continuation, :text, :binary]

  def opcodes, do: Map.keys(@opcodes)

  def new_mask, do: :crypto.strong_rand_bytes(4)

  @spec encode(shorthand_frame() | frame(), [Extension.t()]) ::
          {:ok, binary(), [Extension.t()]} | {:error, any()}
  def encode(frame, mask \\ true, extensions \\ []) when is_friendly_frame(frame) do
    {frame, extensions} =
      frame
      |> translate(mask)
      |> Extension.encode(extensions)

    frame = encode_to_binary(frame)

    {:ok, frame, extensions}
  catch
    :throw, {:mint, reason} -> {:error, reason}
  end

  @spec encode_to_binary(shorthand_frame() | frame()) :: binary()
  defp encode_to_binary(frame) do
    payload = payload(frame)
    mask = mask(frame)
    masked? = if mask == nil, do: 0, else: 1
    encoded_payload_length = encode_payload_length(elem(frame, 0), byte_size(payload))

    <<
      encode_fin(frame)::bitstring,
      reserved(frame)::bitstring,
      encode_opcode(frame)::bitstring,
      masked?::size(1),
      encoded_payload_length::bitstring,
      mask || <<>>::binary,
      apply_mask(payload, mask)::bitstring
    >>
  end

  defp payload(close(code: nil, reason: nil)) do
    <<>>
  end

  defp payload(close(code: code, reason: reason)) do
    code = code || 1_000
    reason = reason || ""
    <<code::unsigned-integer-size(8)-unit(2), reason::binary>>
  end

  for type <- Map.keys(@opcodes) -- [:close] do
    defp payload(unquote(type)(data: data)), do: data
  end

  for type <- Map.keys(@opcodes) do
    defp mask(unquote(type)(mask: mask)), do: mask
    defp reserved(unquote(type)(reserved: reserved)), do: reserved
  end

  defp encode_fin(text(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(binary(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(continuation(fin?: false)), do: <<0b0::size(1)>>
  defp encode_fin(_), do: <<0b1::size(1)>>

  defp encode_opcode(frame), do: @opcodes[elem(frame, 0)]

  def encode_payload_length(_opcode, length) when length in 0..125 do
    <<length::integer-size(7)>>
  end

  def encode_payload_length(opcode, length)
      when length in 126..65_535 and opcode in @non_control_opcodes do
    <<126::integer-size(7), length::unsigned-integer-size(8)-unit(2)>>
  end

  def encode_payload_length(opcode, length)
      when length in 65_535..9_223_372_036_854_775_807 and opcode in @non_control_opcodes do
    <<127::integer-size(7), length::unsigned-integer-size(8)-unit(8)>>
  end

  def encode_payload_length(_opcode, _length) do
    throw({:mint, %Error{reason: :payload_too_large}})
  end

  # Mask the payload by bytewise XOR-ing the payload bytes against the mask
  # bytes (where the mask bytes repeat).
  # This is an "involution" function: applying the mask will mask
  # the data and applying the mask again will unmask it.
  def apply_mask(payload, mask, acc \\ <<>>)

  def apply_mask(payload, nil, _acc), do: payload

  # n=4 is the happy path
  # n=3..1 catches cases where the remaining byte_size/1 of the payload is shorter
  # than the mask
  for n <- 4..1 do
    def apply_mask(
          <<part_key::integer-size(8)-unit(unquote(n)), payload_rest::binary>>,
          <<mask_key::integer-size(8)-unit(unquote(n)), _::binary>> = mask,
          acc
        ) do
      apply_mask(
        payload_rest,
        mask,
        <<acc::binary, :erlang.bxor(mask_key, part_key)::integer-size(8)-unit(unquote(n))>>
      )
    end
  end

  def apply_mask(<<>>, _mask, acc), do: acc

  @spec decode([binary()], binary()) ::
          {:ok, [Extension.t()], [WebSocket.frame() | {:error, term()}]}
          | {:error, any()}
  def decode(frames, extensions \\ []) do
    {extensions, frames} =
      Enum.reduce(frames, {extensions, []}, fn
        {:error, reason}, {extensions, acc} ->
          {extensions, [{:error, reason} | acc]}

        frame, {extensions, acc} ->
          {frame, extensions} = Extension.decode(frame, extensions)

          {extensions, [translate(frame) | acc]}
      end)

    {:ok, extensions, :lists.reverse(frames)}
  catch
    {:mint, reason} -> {:error, reason}
  end

  def binary_to_frames(fragment, buffer, data) do
    case buffer |> Utils.maybe_concat(data) |> decode_raw([]) do
      {:ok, frames} ->
        {fragment, frames} = resolve_fragments(fragment, frames)
        {<<>>, fragment, frames}

      {:buffer, partial, frames} ->
        {fragment, frames} = resolve_fragments(fragment, frames)
        {partial, fragment, frames}
    end
  end

  defp decode_raw(
         <<fin::size(1), reserved::bitstring-size(3), opcode::bitstring-size(4), masked::size(1),
           payload_and_mask::bitstring>> = data,
         acc
       ) do
    case decode_payload_and_mask(payload_and_mask, masked == 0b1) do
      {:ok, payload, mask, rest} ->
        frame = decode_full_frame_binary(opcode, fin, reserved, mask, payload)

        decode_raw(rest, [frame | acc])

      {:error, reason} ->
        {:ok, :lists.reverse([{:error, reason} | acc])}

      :buffer ->
        {:buffer, data, :lists.reverse(acc)}
    end
  end

  defp decode_raw(<<>>, acc), do: {:ok, :lists.reverse(acc)}

  defp decode_raw(partial, acc) when is_binary(partial) do
    {:buffer, partial, :lists.reverse(acc)}
  end

  defp decode_payload_and_mask(payload, masked?) do
    with {:ok, payload_length, rest} <- decode_payload_length(payload),
         {:ok, mask, rest} <- decode_mask(rest, masked?),
         <<payload::binary-size(payload_length), more::bitstring>> <- rest do
      {:ok, payload, mask, more}
    else
      partial when is_binary(partial) -> :buffer
      :buffer -> :buffer
      {:error, reason} -> {:error, reason}
    end
  end

  defp decode_full_frame_binary(opcode, fin, reserved, mask, payload) do
    with {:ok, opcode} <- decode_opcode(opcode) do
      into_frame(
        opcode,
        _fin? = fin == 0b1,
        reserved,
        mask,
        apply_mask(payload, mask)
      )
    end
  end

  defp decode_opcode(opcode) do
    with :error <- Map.fetch(@reverse_opcodes, opcode) do
      {:error, {:unsupported_opcode, opcode}}
    end
  end

  defp decode_payload_length(
         <<127::integer-size(7), payload_length::unsigned-integer-size(8)-unit(8),
           rest::bitstring>>
       ),
       do: {:ok, payload_length, rest}

  defp decode_payload_length(<<127::integer-size(7)>>), do: :buffer

  defp decode_payload_length(
         <<126::integer-size(7), payload_length::unsigned-integer-size(8)-unit(2),
           rest::bitstring>>
       ),
       do: {:ok, payload_length, rest}

  defp decode_payload_length(<<126::integer-size(7)>>), do: :buffer

  defp decode_payload_length(<<payload_length::integer-size(7), rest::bitstring>>)
       when payload_length in 0..125,
       do: {:ok, payload_length, rest}

  defp decode_payload_length(malformed) do
    {:error, {:malformed_payload_length, malformed}}
  end

  defp decode_mask(payload, masked?)

  defp decode_mask(<<mask::binary-size(8)-unit(4), rest::bitstring>>, true) do
    {:ok, mask, rest}
  end

  defp decode_mask(payload, false) do
    {:ok, nil, payload}
  end

  defp decode_mask(payload, _masked?) do
    {:error, {:missing_mask, payload}}
  end

  for data_type <- [:continuation, :text, :binary, :ping, :pong] do
    def into_frame(unquote(data_type), fin?, reserved, mask, payload) do
      unquote(data_type)(
        fin?: fin?,
        reserved: reserved,
        mask: mask,
        data: payload
      )
    end
  end

  def into_frame(
        :close,
        fin?,
        reserved,
        mask,
        <<code::unsigned-integer-size(8)-unit(2), reason::binary>> = payload
      )
      when byte_size(reason) in 0..123 and is_valid_close_code(code) do
    if String.valid?(reason) do
      close(reserved: reserved, mask: mask, code: code, reason: reason, fin?: fin?)
    else
      {:error, {:invalid_close_payload, payload}}
    end
  end

  def into_frame(
        :close,
        fin?,
        reserved,
        mask,
        <<>>
      ) do
    close(reserved: reserved, mask: mask, code: 1_000, reason: "", fin?: fin?)
  end

  def into_frame(
        :close,
        _fin?,
        _reserved,
        _mask,
        payload
      ) do
    {:error, {:invalid_close_payload, payload}}
  end

  # translate from user-friendly tuple into record defined in this module
  # (and the reverse)
  @spec translate(WebSocket.frame() | WebSocket.shorthand_frame()) :: tuple()
  @spec translate(tuple) :: WebSocket.frame()
  for opcode <- Map.keys(@opcodes) do
    def translate(unquote(opcode)(reserved: <<reserved::bitstring>>), _mask?)
        when reserved != <<0::size(3)>> do
      {:error, {:malformed_reserved, reserved}}
    end
  end

  def translate({:error, reason}, _mask?), do: {:error, reason}

  def translate({:text, text}, mask?) do
    mask = if mask?, do: new_mask(), else: nil
    text(fin?: true, mask: mask, data: text)
  end

  def translate({:binary, binary}, mask?) do
    mask = if mask?, do: new_mask(), else: nil
    binary(fin?: true, mask: mask, data: binary)
  end

  def translate(:ping, mask?), do: translate({:ping, <<>>}, mask?)

  def translate({:ping, body}, mask?) do
    mask = if mask?, do: new_mask(), else: nil
    ping(mask: mask, data: body)
  end

  def translate(:pong, mask?), do: translate({:pong, <<>>}, mask?)

  def translate({:pong, body}, mask?) do
    mask = if mask?, do: new_mask(), else: nil
    pong(mask: mask, data: body)
  end

  def translate(:close, mask?) do
    translate({:close, nil, nil}, mask?)
  end

  def translate({:close, code, reason}, mask?) when is_nil(code) or is_valid_close_code(code) do
    mask = if mask?, do: new_mask(), else: nil
    close(mask: mask, code: code, reason: reason, data: <<>>)
  end

  def translate(text(fin?: true, data: data)) do
    if String.valid?(data) do
      {:text, data}
    else
      {:error, {:invalid_utf8, data}}
    end
  end

  def translate(binary(fin?: true, data: data)), do: {:binary, data}
  def translate(ping(data: data)), do: {:ping, data}
  def translate(pong(data: data)), do: {:pong, data}

  def translate(close(code: code, reason: reason)) do
    {:close, code, reason}
  end

  @doc """
  Emits frames for any finalized fragments and stores any unfinalized fragments
  in the `:fragment` key in the websocket data structure
  """
  def resolve_fragments(fragment, frames, acc \\ [])

  def resolve_fragments(fragment, [], acc) do
    {fragment, :lists.reverse(acc)}
  end

  def resolve_fragments(fragment, [{:error, reason} | rest], acc) do
    resolve_fragments(fragment, rest, [{:error, reason} | acc])
  end

  def resolve_fragments(fragment, [frame | rest], acc)
      when is_control(frame) and is_fin(frame) do
    resolve_fragments(fragment, rest, [frame | acc])
  end

  def resolve_fragments(fragment, [frame | rest], acc) when is_fin(frame) do
    frame = combine(fragment, frame)
    resolve_fragments(nil, rest, [frame | acc])
  end

  def resolve_fragments(fragment, [frame | rest], acc) do
    case combine(fragment, frame) do
      {:error, reason} ->
        resolve_fragments(nil, rest, [{:error, reason} | acc])

      frame ->
        resolve_fragments(frame, rest, acc)
    end
  end

  defp combine(nil, continuation(fin?: true)), do: {:error, :insular_continuation}

  defp combine(nil, frame), do: frame

  for type <- [:continuation, :text, :binary] do
    defp combine(
           unquote(type)(data: frame_data) = frame,
           continuation(data: continuation_data, fin?: fin?)
         ) do
      unquote(type)(frame, data: Utils.maybe_concat(frame_data, continuation_data), fin?: fin?)
    end
  end

  defp combine(a, b), do: {:error, {:cannot_combine_frames, a, b}}
end
