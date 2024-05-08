all: build run

build:
	dotnet build

run:
	./src/app/bin/Debug/net8.0/app testProgram.CUM
