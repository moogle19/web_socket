defmodule WebSocket.ErrorTest do
  use ExUnit.Case

  alias WebSocket.Error

  test "error reason handling" do
    assert %Error{reason: :extended_connect_disabled} |> Error.message() ==
             "extended CONNECT method not enabled"

    assert %Error{reason: :payload_too_large} |> Error.message() ==
             "frame payload cannot exceed 9,223,372,036,854,775,807 bytes"

    assert %Error{reason: {:extension_not_negotiated, "some extension"}} |> Error.message() ==
             "the remote server accepted an extension the client did not offer: \"some extension\""

    assert_raise FunctionClauseError, fn ->
      %Error{reason: :unknown_error} |> Error.message()
    end
  end
end
