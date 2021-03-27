defmodule NaappleTest do
  use ExUnit.Case
  doctest Naapple

  test "greets the world" do
    assert Naapple.hello() == :world
  end
end
