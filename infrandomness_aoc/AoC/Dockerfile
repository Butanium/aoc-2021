FROM mcr.microsoft.com/dotnet/runtime:6.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:6.0 AS build
WORKDIR /src
COPY ["AoC.csproj", "AoC/"]
RUN dotnet restore "AoC/AoC.csproj"
COPY . .
WORKDIR "/src/AoC"
RUN dotnet build "AoC.csproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "AoC.csproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "AoC.dll"]
