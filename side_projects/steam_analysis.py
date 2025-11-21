import string
from collections import Counter
import requests
import operator
import os
from utils.cachify import LocalCache
from dotenv import load_dotenv
import datetime as dt
from matplotlib import pyplot as plt
import numpy as np

API_ENDPOINTS = {
    "Playtime": "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/",
    "GameMetadata": "https://store.steampowered.com/api/appdetails",
    "TagData": "https://steamspy.com/api.php?request=appdetails",
    "Achievements": (
        "http://api.steampowered.com/ISteamUserStats/GetPlayerAchievements/v0001/"
    ),
    "AchievementMetadata": (
        "http://api.steampowered.com/ISteamUserStats/GetSchemaForGame/v2/"
    ),
}
DATE_FORMATS = ["%d %b, %Y", "%B %d, %Y"]
cache = LocalCache(os.environ.get("CACHIFY_PATH", f"./cachify"))
load_dotenv()
STEAM_API_KEY = os.getenv("STEAM_API_KEY")
STEAM_ID_64 = os.getenv("STEAM_ID_64")
RESULT_FOLDER = "steam_analysis_graphs/"


KNOWN_MISSING_GAMES = [
    "Half-Life 2: Episode One",
    "Half-Life 2: Episode Two",
    "Logicality",
    "Puzzle Park",
    "Gravitas",
    "Re:Touring",
    "Door3:Insignia",
    "Door4:Ultimatum",
    "ReThink 2",
    "ReThink 3",
    "ReThink 4",
    "ReThink | Evolved 3",
    "ReThink | Lite",
]

MISSING_RELEASE_DATES = {
    "Deadpool": dt.datetime(2013, 6, 25, 0, 0),
    "1... 2... 3... KICK IT! (Drop That Beat Like an Ugly Baby)": dt.datetime(
        2011, 4, 1, 0, 0
    ),
    "Myst": dt.datetime(2021, 8, 26, 0, 0),
}


@cache.cachify(lifetime=7)
def ping_API(url: str, params: dict):
    print("Pinging Steam API...")

    try:
        response = requests.get(url, params=params)
        response.raise_for_status()  # Raise an HTTPError for bad responses (4xx or 5xx)
        data = response.json()

    except requests.exceptions.RequestException as e:
        print(f"Error accessing Steam API: {e}")
        return None

    print("Got a response.")

    return data


def get_game_playtime(threshold: int = 1):
    # Parameters for the API request
    params = {
        "key": STEAM_API_KEY,
        "steamid": STEAM_ID_64,
        "format": "json",
        "include_appinfo": 1,  # Includes game name and icon data
        "include_played_free_games": (
            1  # Includes free-to-play games that have been played
        ),
    }
    response = ping_API(API_ENDPOINTS["Playtime"], params)["response"]
    games_list = response["games"]

    # Sort the games by 'playtime_forever' (in minutes) in descending order
    # The playtime is in minutes, so we convert it to hours for display later
    sorted_games = sorted(
        games_list, key=operator.itemgetter("playtime_forever"), reverse=True
    )

    return [game for game in sorted_games if game["playtime_forever"] > threshold]


def get_game_achievements(games, threshold: float = 1.0):
    result = dict()
    for game in games:
        # Parameters for the API request
        # params = {
        #     "key": STEAM_API_KEY,
        #     "appid": appid,
        #     "l": "english",
        # }
        # response = ping_API(API_ENDPOINTS["AchievementMetadata"], params)
        # achievements = response['game']['availableGameStats']['achievements']

        # Parameters for the API request
        params = {
            "key": STEAM_API_KEY,
            "steamid": STEAM_ID_64,
            "appid": game["appid"],
            "l": "english",
        }
        response = ping_API(API_ENDPOINTS["Achievements"], params)
        if (
            response is None
            or "playerstats" not in response
            or "achievements" not in response["playerstats"]
        ):  # Game has no achievements
            print(f"Could not find achievements for {game['name']}")
            if threshold == 0:
                result.update(
                    {
                        game["appid"]: {
                            "name": game["name"],
                            "playtime": game["playtime_forever"],
                            "visible_stats": game["has_community_visible_stats"],
                            "total_achieves": 0,
                            "achieved": 0,
                        }
                    }
                )
            continue

        achievements = response["playerstats"]["achievements"]

        n_achieved = sum([achieve["achieved"] for achieve in achievements])
        rate = n_achieved / len(achievements)
        if not response["playerstats"]["success"]:
            breakpoint()
        if rate >= threshold:
            result.update(
                {
                    game["appid"]: {
                        "name": game["name"],
                        "playtime": game["playtime_forever"],
                        "visible_stats": game["has_community_visible_stats"],
                        "total_achieves": len(achievements),
                        "achieved": n_achieved,
                    }
                }
            )

    return result


def get_game_metadata(games):
    for appid, game in games.items():
        # Parameters for the API request
        params = {"appid": appid}
        response_spy = ping_API(API_ENDPOINTS["TagData"], params=params)

        if game["name"] in MISSING_RELEASE_DATES.keys():
            date = MISSING_RELEASE_DATES[game["name"]]
            genres = []
        else:
            # Parameters for the API request
            params = {"appids": appid}
            response_steam = ping_API(API_ENDPOINTS["GameMetadata"], params=params)

            date = None
            genres = []
            if (
                not response_steam is None
                and str(appid) in response_steam
                and "data" in response_steam[str(appid)]
            ):
                genres = [
                    genre["description"]
                    for genre in response_steam[str(appid)]["data"]["genres"]
                ]
                for fmt in DATE_FORMATS:
                    try:
                        date = dt.datetime.strptime(
                            response_steam[str(appid)]["data"]["release_date"]["date"],
                            fmt,
                        )
                    except ValueError:
                        continue

            if date is None:
                print(f"Could not find release date for {game['name']}")
            if genres is []:
                print(f"Could not find genres for {game['name']}")

        game.update(
            {
                "tags": response_spy["tags"],
                "genres": genres,
                "date": date,
            }
        )

    return games


def get_tag_counts(games, top_tags_per_game=0, tag_threshold=0):
    tag_counts = dict(
        Counter(
            [
                tag
                for game_tags in [
                    (
                        list(game["tags"].keys())[:top_tags_per_game]
                        if top_tags_per_game > 0
                        else list(game["tags"].keys())
                    )
                    for game in games.values()
                    if len(game["tags"]) > 0
                ]
                for tag in game_tags
            ]
        )
    )
    filtered_tag_counts = {
        tag: count for tag, count in tag_counts.items() if count >= tag_threshold
    }
    return dict(sorted(filtered_tag_counts.items(), key=lambda x: x[1], reverse=True))


def get_genre_counts(games, genre_threshold=0):
    genre_counts = dict(
        Counter(
            [
                genre
                for game_genres in [
                    game["genres"] for game in games.values() if len(game["genres"]) > 0
                ]
                for genre in game_genres
            ]
        )
    )
    filtered_genre_counts = {
        genre: count
        for genre, count in genre_counts.items()
        if count >= genre_threshold
    }
    return dict(sorted(filtered_genre_counts.items(), key=lambda x: x[1], reverse=True))


def get_playtime_per_tag(games, top_tags_per_game=0, tag_threshold=0):
    tag_playtime = {
        tag: 0
        for game in [game["tags"] for game in games.values() if len(game["tags"]) > 0]
        for tag in game.keys()
    }
    for game in games.values():
        if len(game["tags"]) > 0:
            if top_tags_per_game == 0:
                for tag in list(game["tags"].keys()):
                    tag_playtime[tag] = tag_playtime[tag] + game["playtime"] / 60
            else:
                for tag in list(game["tags"].keys())[:top_tags_per_game]:
                    tag_playtime[tag] = tag_playtime[tag] + game["playtime"] / 60
    filtered_tag_playtime = {
        tag: time for tag, time in tag_playtime.items() if time >= tag_threshold
    }
    return dict(sorted(filtered_tag_playtime.items(), key=lambda x: x[1], reverse=True))


def make_histo(
    data,
    title: str,
    xlabel: str,
    min_value: int = None,
    max_value: int = None,
    bin_size: int = 1,
    show_top: int = 0,
    top_text: str = "",
    reverse_top: bool = False,
):
    """
    Generates a labeled histogram with a special final bin for outliers.
    """
    data_values = data.values()

    # 1. Prepare Data for Binning
    # Separate data_values into two groups: standard and outliers
    if not max_value is None:
        standard_data_values = [p for p in data_values if p < max_value]
        outlier_count = len(data_values) - len(standard_data_values)
    else:
        standard_data_values = list(data_values)
        outlier_count = 0
        max_value = max(data_values)

    # 2. Determine Bin Edges for Standard Data
    if min_value is None:
        min_value = min(data_values)

    # The last standard bin edge must be exactly max_value
    # Use standard_data_values to ensure the last bin covers up to max_value
    max_bin_limit = max_value

    # Create bin edges up to the max_value (e.g., [0, 50, 100, ..., 1000])
    bin_edges = np.arange(min_value, max_bin_limit + bin_size, bin_size)

    # Ensure the last edge is max_value, which might be slightly different
    # if max_value is not a perfect multiple of bin_size.
    # We only care about the bins up to max_value.
    bin_edges = bin_edges[bin_edges <= max_value]

    # 3. Calculate Histogram Counts

    # Calculate counts for the standard bins
    hist, _ = np.histogram(standard_data_values, bins=bin_edges)

    # Add the outlier count to the end of the histogram counts
    final_hist = np.append(hist, outlier_count)

    # 4. Create Custom X-axis Labels
    x_labels = []
    # Labels for standard bins
    for i in range(len(bin_edges) - 1):
        lower = int(bin_edges[i])
        upper = int(bin_edges[i + 1])
        if bin_size == 1:
            x_labels.append(f"{lower}")
        else:
            x_labels.append(f"{lower}-{upper}")

    # Label for the final outlier bin
    if max(data_values) <= max_value:
        x_labels.append(f"{max_value}")
    else:
        x_labels.append(f"{max_value}+")

    # 5. Plot the Data

    # x_positions must cover all bins, including the outlier bin
    x_positions = np.arange(len(final_hist))

    if show_top > 0:
        fig, (ax1, ax2) = plt.subplots(
            nrows=2, ncols=1, figsize=(12, 8), gridspec_kw={"height_ratios": [4, 1]}
        )
    else:
        fig, ax1 = plt.subplots(nrows=1, ncols=1, figsize=(12, 8))

    plot_histo(ax1, x_positions, final_hist, x_labels, title, xlabel)

    if show_top > 0:
        # Add game list
        top_data = sorted(data.items(), key=lambda x: x[1], reverse=not reverse_top)[
            :show_top
        ]

        # Hide the axes for the text subplot
        ax2.axis("off")

        # Format the top games list into a single string
        top_games_text = f"{top_text}\n"

        for i, (name, playtime) in enumerate(top_data):
            top_games_text += f"{i+1}. {name}: {playtime:.0f}\n"

        # Add the formatted text to the subplot
        ax2.text(
            0.0,
            1.0,  # Position the text (bottom left corner of the subplot)
            top_games_text,
            transform=ax2.transAxes,  # Use axis coordinates
            fontsize=10,
            verticalalignment="top",
            family="monospace",  # Use monospace for better alignment
        )

    plt.tight_layout()
    # plt.show()
    fig.savefig(RESULT_FOLDER + title)


def make_histo_lean(data, title: str, x_axis_label: str):
    fig, ax1 = plt.subplots(nrows=1, ncols=1, figsize=(12, 8))
    plot_histo(
        ax1,
        x_positions=range(len(data)),
        final_hist=data.values(),
        x_labels=data.keys(),
        title=title,
        x_axis_label=x_axis_label,
    )
    plt.tight_layout()
    # plt.show()
    fig.savefig(RESULT_FOLDER + title)


def plot_histo(
    ax,
    x_positions,
    final_hist,
    x_labels,
    title: str,
    x_axis_label: str,
):
    # Make historgram
    # Plot using the final_hist array
    bars = ax.bar(x_positions, final_hist, width=0.8, color="teal", edgecolor="black")

    # Apply the custom labels to the X-axis ticks
    ax.set_xticks(x_positions, x_labels, rotation=90, ha="center")
    ax.set_xticklabels(x_labels, rotation=90, ha="center")
    ax.tick_params(axis="x", which="major", pad=-1)

    # Add labels and title
    ax.set_title(title)
    ax.set_xlabel(x_axis_label)
    ax.set_ylabel("Number of Games")
    ax.grid(axis="y", linestyle="--", alpha=0.7)

    # Add the count value on top of each bar
    for bar in bars:
        height = bar.get_height()
        if height > 0:
            ax.text(
                bar.get_x() + bar.get_width() / 2.0,
                height,
                "%d" % int(height),
                ha="center",
                va="bottom",
            )


def make_str_histo(game_names: list):
    """
    Creates a bar chart showing the frequency of games based on their starting letter.
    """

    # 1. Classify and Count Starting Characters

    # Use a dictionary to store the counts
    letter_counts = Counter()

    for name in game_names:
        if not name:
            continue

        first_char = name[0]

        if first_char.isalpha():
            # If alphabetic, count the uppercase letter (A-Z)
            letter_counts[first_char.upper()] += 1
        else:
            # If non-alphabetic (number, symbol, etc.), put it in the 'Other' bin
            letter_counts["0-9"] += 1

    # 2. Prepare Data for Plotting

    # Define the full set of X-axis labels
    # Start with A-Z
    alphabet_labels = list(string.ascii_uppercase)

    # Append the final bin for non-alphabetic characters
    final_labels = alphabet_labels + ["0-9"]

    # Get the counts for each label, defaulting to 0 for missing letters
    counts = [letter_counts.get(label, 0) for label in final_labels]

    # 3. Plotting the Bar Chart
    fig, ax1 = plt.subplots(nrows=1, ncols=1, figsize=(14, 6))

    # Use ax1.bar()
    bars = ax1.bar(final_labels, counts, color="darkorange", edgecolor="black")

    # Add labels and title
    ax1.set_title("Game Library Distribution by Starting Character")
    ax1.set_xlabel("Starting Character of Game Title")
    ax1.set_ylabel("Number of Games")
    ax1.grid(axis="y", linestyle="--", alpha=0.7)

    # Add count values on top of each bar
    for bar in bars:
        height = bar.get_height()
        if height > 0:
            ax1.text(
                bar.get_x() + bar.get_width() / 2.0,
                height,
                "%d" % int(height),
                ha="center",
                va="bottom",
            )

    plt.tight_layout()
    # plt.show()
    fig.savefig(RESULT_FOLDER + "Starting Character")


if __name__ == "__main__":
    # Get data
    games_with_playtime = get_game_playtime(threshold=1)
    games_with_achievements = get_game_achievements(games_with_playtime, threshold=1.0)
    games_with_metadata = get_game_metadata(games_with_achievements)

    # Process data
    top_tag_counts = get_tag_counts(games_with_metadata, 3, 2)
    all_tag_counts = get_tag_counts(games_with_metadata, 0, 10)
    genre_counts = get_genre_counts(games_with_metadata, 0)
    playtime_per_tag = get_playtime_per_tag(games_with_metadata, 5, 100)

    # Plot data
    make_histo(
        data={
            game["name"]: game["playtime"] / 60 for game in games_with_metadata.values()
        },
        title="Playtime",
        xlabel="Playtime",
        bin_size=10,
        max_value=250,
        show_top=10,
        top_text="**Top 10 Games by playtime in hours**",
    )
    make_histo(
        data={
            game["name"]: game["total_achieves"]
            for game in games_with_metadata.values()
        },
        title="Number of Achievements",
        xlabel="Achievements",
        bin_size=5,
        max_value=100,
        show_top=5,
        top_text="**Top 5 Games by number of achievements**",
    )
    make_histo(
        data={game["name"]: game["date"].year for game in games_with_metadata.values()},
        title="Release Year",
        xlabel="Year",
    )
    make_histo(
        data={
            game["name"]: game["playtime"] / game["total_achieves"]
            for game in games_with_metadata.values()
        },
        title="Playtime per Achievement",
        xlabel="Minutes/Achievement",
        bin_size=10,
        min_value=0,
        max_value=300,
        show_top=10,
        top_text="**Top 10 Games by time per achievement**",
    )
    make_str_histo([game["name"] for game in games_with_metadata.values()])
    make_histo_lean(top_tag_counts, "Top Tag Counts", "Tag")
    make_histo_lean(all_tag_counts, "Total Tag Counts", "Tag")
    make_histo_lean(genre_counts, "Genre Counts", "Genre")
    make_histo_lean(playtime_per_tag, "Playtime per Tag", "Tag")
