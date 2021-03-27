defmodule Naapple.Napster.API do
  @doc_api_key "YTkxZTRhNzAtODdlNy00ZjMzLTg0MWItOTc0NmZmNjU4Yzk4"
  @api_url "https://api.napster.com/v2.2/"

  def get_api_key() do
    @doc_api_key
  end

  def get_api_url() do
    @api_url
  end

end
