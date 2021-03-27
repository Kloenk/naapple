defmodule Naapple.Napster.API.Source do
  def get_url(url, apikey, opt_headers \\ [], config \\ []) when is_binary(url) and is_binary(apikey) do
    headers = [apikey: apikey] ++ opt_headers
    HTTPoison.get(url, headers, config)
    |> parse_body
  end

  defp parse_body({:ok, body}) do
    body.body
    |> Poison.decode
  end

  defp parse_body(e), do: e
end


defmodule Naapple.Napster.API.Source.Track do
  alias Naapple.Napster.API

  defstruct apikey: API.get_api_key(),
    id: nil,
    name: nil,
    disc: nil,
    seconds: nil,
    explicit: nil,
    isrc: nil,
    artist: nil,
    album: nil

end

defmodule Naapple.Napster.API.Source.Playlist do
  alias Naapple.Napster.API

  defstruct apikey: API.get_api_key(),
    playlist: nil,
    name: nil,
    track_count: nil,
    description: nil,
    modified: nil

  def update(playlist) do
    if !playlist.playlist do
      {:error, "no playlist set"}
    end

    API.get_api_url <> "playlists/" <> playlist.playlist
    |> API.Source.get_url(playlist.apikey)
    |> update_json(playlist)

  end

  def update_json({:ok, json}, playlist) do
    [ json_playlist | _ ] = Map.get(json, "playlists")

    playlist = playlist
    |> Map.put(:name, Map.get(json_playlist, "name"))
    |> Map.put(:track_count, Map.get(json_playlist, "trackCount"))
    |> Map.put(:description, Map.get(json_playlist, "description"))
    |> Map.put(:modified, Map.get(json_playlist, "modified"))

    {:ok, playlist}
  end

  def update_json(e, _playlist), do: e

  def get_all_tracks(playlist, offset \\ 0, limit \\ 0) when
  offset >= 0 and
  offset < playlist.track_count and
  is_number(playlist.track_count) and
  limit >= 0 and
  limit <= playlist.track_count do
    limit = if limit == 0, do: playlist.track_count, else: limit
    limit = floor((limit - offset) / 200)

    0..limit
    |> Stream.map(&(&1 * 200 + offset))
    |> Stream.map(&get_track_chunk(playlist, &1, 200))
    |> Stream.concat
  end

  def get_track_chunk(playlist, offset \\ 0, limit \\ 200) when
  offset >= 0 and
  #offset < (playlist.track_count - 1) and
  limit > 0 and limit <= 200 do
    {:ok, body} = API.get_api_url <> "playlists/" <> playlist.playlist <> "/tracks?offset=#{offset}&limit=#{limit}"
    |> API.Source.get_url(playlist.apikey) # TODO: do I need meta?

    body
    |> Map.get("tracks")
    |> Stream.map(&parse(&1, playlist))
  end

  def parse(json, playlist = %API.Source.Playlist{}) when is_map(json) do
    #json
    %API.Source.Track{
      apikey: playlist.apikey,
      id: Map.get(json, "id"),
      name: Map.get(json, "name"),
      disc: Map.get(json, "disc"),
      seconds: Map.get(json, "playbackSeconds"),
      explicit: Map.get(json, "isExplicit"),
      isrc: Map.get(json, "isrc"),
      artist: Map.get(json, "artistName"),
      album: Map.get(json, "albumName")
    }
  end
end
