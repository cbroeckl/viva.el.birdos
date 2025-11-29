import numpy as np
import pandas as pds
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter

TOP_WAR_CUTOFF = 4.0
MID_WAR_MIN = 1.5
MID_WAR_MAX = 3.0


def top_tier_pitcher_war(x):
    return (x >= TOP_WAR_CUTOFF).sum()


def top_tier_pitcher_war_pct(x):
    return top_tier_pitcher_war(x) / len(x)


def mid_tier_pitcher_war(x):
    return ((x <= MID_WAR_MAX) & (x >= MID_WAR_MIN)).sum()


def mid_tier_pitcher_war_pct(x):
    return mid_tier_pitcher_war(x) / len(x)


def add_trendline(ax, x, y, label, color, **kwargs):
    """Add polynomial trendline to plot."""
    z = np.polyfit(x, y, 1)
    p = np.poly1d(z)
    ax.plot(x, p(x), linestyle="--", alpha=0.7, label=label, color=color, **kwargs)


def main():
    # Exported from Fangraphs by hand, using Leaders > Pitching
    # Positional Split: SP
    # Min Innings: 100
    # Years 2008-2025
    # Separate seasons
    pitchers_df = pds.read_csv("../data/fg-pitchers-2008-2025.csv")

    # pitchers_df.drop(pitchers_df[pitchers_df["Season"] == 2021].index, inplace=True)

    compiled_stats = (
        pitchers_df.groupby("Season")
        .agg(
            num_pitchers=("PlayerId", "count"),
            max_war=("WAR", "max"),
            four_plus_war=("WAR", top_tier_pitcher_war),
            one_and_a_half_to_three_war=("WAR", mid_tier_pitcher_war),
            top_war_pct=("WAR", top_tier_pitcher_war_pct),
            mid_war_pct=("WAR", mid_tier_pitcher_war_pct),
        )
        .reset_index()
    )

    compiled_stats["top_war_pct_fmt"] = compiled_stats["top_war_pct"].apply(
        "{:.2%}".format
    )
    compiled_stats["mid_war_pct_fmt"] = compiled_stats["mid_war_pct"].apply(
        "{:.2%}".format
    )

    fig, ax = plt.subplots(figsize=(12, 6))
    plt.plot(
        compiled_stats["Season"],
        compiled_stats["mid_war_pct"],
        marker="o",
        label="Mid WAR %",
        linewidth=2,
        color="firebrick",
    )
    plt.plot(
        compiled_stats["Season"],
        compiled_stats["top_war_pct"],
        marker="s",
        label="Top WAR %",
        linewidth=2,
        color="tab:blue",
    )

    # Add trend lines
    add_trendline(
        ax,
        compiled_stats["Season"],
        compiled_stats["mid_war_pct"],
        "Mid WAR % Trend",
        "salmon",
    )

    add_trendline(
        ax,
        compiled_stats["Season"],
        compiled_stats["top_war_pct"],
        "Top WAR % Trend",
        "tab:cyan",
    )

    plt.xlabel("Season")
    plt.ylabel("% of Pitchers in Bucket")
    plt.title("Mid vs Top WAR Percentage by Season")
    plt.legend()
    plt.grid(True, alpha=0.3)

    # Force integer x-axis ticks
    plt.xticks(compiled_stats["Season"])
    # Format y-axis as percentages
    plt.gca().yaxis.set_major_formatter(PercentFormatter(xmax=1.0, decimals=0))

    for idx, row in compiled_stats.iterrows():
        plt.annotate(
            f"{row['mid_war_pct']:.2%}",
            xy=(row["Season"], row["mid_war_pct"]),
            xytext=(0, -15),  # 5 points above
            textcoords="offset points",
            ha="center",
            fontsize=8,
            color="black",
        )

        plt.annotate(
            f"{row['top_war_pct']:.2%}",
            xy=(row["Season"], row["top_war_pct"]),
            xytext=(0, -15),  # 5 points below
            textcoords="offset points",
            ha="center",
            fontsize=8,
            color="black",
        )

    plt.tight_layout()
    plt.show()

    markdown_table = compiled_stats.drop(columns=["top_war_pct", "mid_war_pct"])
    print(markdown_table.to_markdown())


if __name__ == "__main__":
    main()
