#!/usr/bin/env bash
set -e

dir=$(dirname "$0")
cd "$dir"

if [ -f .env ]; then
	# shellcheck disable=SC1091
	source .env
fi

export AOC_SESSION_COOKIE
dotnet run --project Kodfodrasz.AoC.Cli/Kodfodrasz.AoC.Cli.fsproj
