defmodule Naapple.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    HTTPoison.start()

    children = [
      # Starts a worker by calling: Naapple.Worker.start_link(arg)
      # {Naapple.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Naapple.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
