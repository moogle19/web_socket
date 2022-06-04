defmodule WebSocket.FrameTest do
  use ExUnit.Case

  alias WebSocket.Frame

  describe "encode WebSocket frames" do
    test "encode text frame without extensions" do
      assert {:ok, <<129, 3, 102, 111, 111>>, []} = Frame.encode({:text, "foo"}, false)
    end

    test "encode binary frame without extensions" do
      assert {:ok, <<130, 3, 102, 111, 111>>, []} =
               Frame.encode({:binary, <<102, 111, 111>>}, false)

      assert {:ok, <<130, 3, 102, 111, 111>>, []} = Frame.encode({:binary, "foo"}, false)
    end

    test "encode ping frame without extensions" do
      assert {:ok, <<137, 0>>, []} = Frame.encode(:ping, false)
    end

    test "encode ping frame with payload and without extensions" do
      assert {:ok, <<137, 4, 112, 105, 110, 103>>, []} = Frame.encode({:ping, "ping"}, false)
    end

    test "encode pong frame without extensions" do
      assert {:ok, <<138, 0>>, []} = Frame.encode(:pong, false)
    end

    test "encode pong frame with payload and without extensions" do
      assert {:ok, <<138, 4, 112, 111, 110, 103>>, []} = Frame.encode({:pong, "pong"}, false)
    end

    test "encode close frame without extensions" do
      assert {:ok, <<136, 0>>, []} = Frame.encode(:close, false)
    end

    test "encode close frame with payload and without extensions" do
      assert {:ok, <<136, 14, 4, 87, 110, 111, 114, 109, 97, 108, 32, 99, 108, 111, 115, 101>>,
              []} = Frame.encode({:close, 1111, "normal close"}, false)
    end
  end

  describe "decode WebSocket frames" do
    test "decode text frame without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<129, 3, 102, 111, 111>>, <<>>)
      assert {:ok, [], [text: "foo"]} = Frame.decode(fragments)
    end

    test "decode binary frame without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<130, 3, 102, 111, 111>>, <<>>)
      assert {:ok, [], [binary: "foo"]} = Frame.decode(fragments)
    end

    test "decode ping frame without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<137, 0>>, <<>>)
      assert {:ok, [], [ping: ""]} = Frame.decode(fragments)
    end

    test "decode ping frame with payload and without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<137, 4, 112, 105, 110, 103>>, <<>>)
      assert {:ok, [], [ping: "ping"]} = Frame.decode(fragments)
    end

    test "decode pong frame without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<138, 0>>, <<>>)
      assert {:ok, [], [pong: ""]} = Frame.decode(fragments)
    end

    test "decode pong frame with payload and without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<138, 4, 112, 111, 110, 103>>, <<>>)
      assert {:ok, [], [pong: "pong"]} = Frame.decode(fragments)
    end

    test "decode close frame without extensions" do
      {"", nil, fragments} = Frame.binary_to_frames(nil, <<136, 0>>, <<>>)
      assert {:ok, [], [{:close, 1000, ""}]} = Frame.decode(fragments)
    end

    test "decode close frame with payload and without extensions" do
      {"", nil, fragments} =
        Frame.binary_to_frames(
          nil,
          <<136, 14, 4, 87, 110, 111, 114, 109, 97, 108, 32, 99, 108, 111, 115, 101>>,
          <<>>
        )

      assert {:ok, [], [{:close, 1111, "normal close"}]} = Frame.decode(fragments)
    end
  end
end
