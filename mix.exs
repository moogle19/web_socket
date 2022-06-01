defmodule WebSocket.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-mint/web_socket"

  def project do
    [
      app: :web_socket,
      version: "0.1.0",
      elixir: "~> 1.8",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      description: description(),
      source_url: @source_url,
      name: "WebSocket",
      docs: docs()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.24", only: [:dev], runtime: false},
      {:credo, "~> 1.0", only: [:test], runtime: false},
      {:excoveralls, "~> 0.14", only: [:test]}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/fixtures"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      name: "mint_web_socket",
      files: ~w(lib .formatter.exs mix.exs README.md .version),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => @source_url <> "/blob/main/CHANGELOG.md"
      }
    ]
  end

  defp description do
    "WebSocket encoding and decoding"
  end

  defp docs do
    [
      deps: [],
      language: "en",
      formatters: ["html"],
      main: WebSocket,
      extras: [
        "CHANGELOG.md"
      ],
      skip_undefined_reference_warnings_on: [
        "CHANGELOG.md"
      ]
    ]
  end
end
