defmodule Naapple.Apple.API.Auth.Token do
  use Joken.Config

  def token_config do
    %{}
    |> add_claim("my_key", fn -> "My custom claim" end, &(&1 == "My custom claim"))
  end

end
