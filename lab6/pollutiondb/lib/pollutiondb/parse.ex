defmodule Parse do
  def parse_line(line) do
    [datetime, type, value, id, name, latlong] = line |> String.split(";")
    {{year, month, day}, {hours, minutes, seconds}} = parse_date(datetime)
    {_, date} = Date.new(year, month, day)
    {_, time} = Time.new(hours, minutes, seconds)
    ogName = name
    {_country, _city, name} = parse_name(name)
    latlong = parse_coords(latlong)

    %{
      :date => date,
      :time => time,
      :location => latlong,
      :stationId => id,
      :stationName => name,
      :stationIdentifier => "#{id}#{ogName}",
      :pollutionType => type,
      :pollutionLevel => value |> String.to_float()
    }
  end

  defp parse_date(date) do
    [date, time] = date |> String.split("T") |> Enum.map(&String.replace(&1, "Z", ""))

    {year, month, day} =
      date |> String.split("-") |> Enum.map(&String.to_integer/1) |> List.to_tuple()

    [hours, minutes, seconds] = time |> String.split(":")
    [hours, minutes] = [hours, minutes] |> Enum.map(&String.to_integer/1)
    seconds = seconds |> String.slice(0..1) |> String.to_integer()
    {{year, month, day}, {hours, minutes, seconds}}
  end

  defp parse_name(name) do
    # {country, city, address}
    for str <- name |> String.split(",") do
      str |> String.trim()
    end
    |> List.to_tuple()
  end

  def parse_coords(coords) do
    # {lat,long}
    coords |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple()
  end

  def read_from_file() do
    file = File.read!("AirlyData-ALL-50k.csv")

    all_lines =
      file
      |> String.split("\n")
      |> Enum.map(&Parse.parse_line/1)

    stations =
      file
      |> String.split("\n")
      |> Enum.map(&Parse.parse_line/1)
      |> Enum.uniq_by(& &1.stationId)

    {all_lines, stations}
  end

  def add_stations() do
    {_, stations} = read_from_file()

    stations
    |> Enum.map(
      &Pollutiondb.Station.add(%Pollutiondb.Station{
        name: &1.stationIdentifier,
        lat: elem(&1.location, 0),
        lon: elem(&1.location, 1)
      })
    )
  end

  def add_values() do
    {values, _} = read_from_file()

    values
    |> Enum.map(
      &Pollutiondb.Reading.add(
        Pollutiondb.Station.find_by_name(&1.stationIdentifier) |> List.first,
        &1.date,
        &1.time,
        &1.pollutionType,
        &1.pollutionLevel
      )
    )
  end

  def add_stations_and_values() do
    add_stations()
    add_values()
  end
end
