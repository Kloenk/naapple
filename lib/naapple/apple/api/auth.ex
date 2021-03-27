defmodule Naapple.Apple.API.Auth do

  def get_signer(file, kid) do
    {:ok, file_content} = File.read(file)

    Joken.Signer.create("ES256", %{"pem" => [file_content]}, %{"kid" => kid})

  end

end
