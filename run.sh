#!/usr/bin/env bash
set -e

dir=$(dirname "$0")
cd "$dir"

if [ -f .env ]; then
	# shellcheck disable=SC1091
	source .env
fi

export AOC_SESSION_COOKIE
export AOC_DATA_DIR=${AOC_DATA_DIR:=Input}
dotnet run --no-launch-profile --project Kodfodrasz.AoC.Cli/Kodfodrasz.AoC.Cli.fsproj
