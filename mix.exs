defmodule Pandora.MixProject do
  use Mix.Project

  def project do
    [
      app: :pandora,
      name: "pandora",
      package: package(),
      description: description(),
      version: "1.0.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [
        main: "Pandora",
        extras: [
          "README.md",
          "CHANGELOG.md",
          "CONTRIBUTING.md"
        ]
      ]
    ]
  end

  def application,
    do: [
      extra_applications: [:logger]
    ]

  defp description do
    "A Simple XML library."
  end

  defp package() do
    [
      name: "pandora",
      files: ~w(
        lib mix.exs README.md CONTRIBUTING.md LICENSE
        CHANGELOG.md test
      ),
      maintainers: ["Angus Karl Stewart Thomsen"],
      licenses: ["MIT"],
      links: %{
        "Github" => "https://github.com/AKST/pandora"
      }
    ]
  end

  def deps do
    [
      {:queue_wrapper, "~> 1.0.0"},
      {:ex_doc, "~> 0.19.1", only: :dev},
      {:dialyxir, "~> 0.5", only: :dev, runtime: false}
    ]
  end
end
