require Ecto.Query

defmodule Pollutiondb.Reading do
  use Ecto.Schema

  schema "readings" do
    field(:date, :date)
    field(:time, :time)
    field(:type, :string)
    field(:value, :float)
    belongs_to(:station, Pollutiondb.Station)
  end

  def add_now(station, type, value) do
    reading = %Pollutiondb.Reading{
      station: station,
      # station_id: station.id,
      date: Date.utc_today,
      time: Time.truncate(Time.utc_now(), :second),
      type: type,
      value: value
    }
    Pollutiondb.Repo.insert(reading)
  end

  def find_byt_date(date) do
    Pollutiondb.Repo.all(Ecto.Query.where(Pollutiondb.Reading, date: ^date))
  end

  def add(station, date, time, type, value) do
    reading = %Pollutiondb.Reading{
      station: station,
      date: date,
      time: time,
      type: type,
      value: value
    }
    Pollutiondb.Repo.insert(reading)
  end
end
