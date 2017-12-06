defmodule Erlyfix.MixProject do
  use Mix.Project

  def project() do
    [
      app: :erlyfix,
      version: "0.1.0",
      language: :erlang,
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      name: "Erlyfix",
      source_url: "https://github.com/basiliscos/erl-erlyfix"
    ]
  end

  def application() do
    []
  end

  defp deps() do
    [
      {:erlsom, "~> 1.4.0"},
      {:ex_doc, "~> 0.14", only: :dev},
    ]
  end

  defp description() do
    "FIX (Foreign Information Exchange) protocol implementation for Erlang."
  end

  defp package() do
    [
      files: ["src",  "mix.exs", "rebar*", "README*", "LICENSE*"],
      maintainers: ["Ivan Baidakou"],
      licenses: ["Apache 2.0"],
      links: %{"GitHub" => "https://github.com/basiliscos/erl-erlyfix"}
    ]
  end
end
