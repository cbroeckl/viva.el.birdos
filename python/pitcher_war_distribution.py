import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
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
    pitchers_df = pd.read_csv("../data/fg-pitchers-2008-2025.csv")

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

    # Reshape data for seaborn (long format)
    plot_data = pd.melt(
        compiled_stats,
        id_vars=["Season"],
        value_vars=["mid_war_pct", "top_war_pct"],
        var_name="WAR_Category",
        value_name="Percentage",
    )

    # Rename categories for better labels
    plot_data["WAR_Category"] = plot_data["WAR_Category"].map(
        {"mid_war_pct": "Mid WAR (1.5-3.0)", "top_war_pct": "Top WAR (4.0+)"}
    )

    # Create figure
    fig, ax = plt.subplots(figsize=(12, 6))

    # Create lineplot with seaborn
    sns.lineplot(
        data=plot_data,
        x="Season",
        y="Percentage",
        hue="WAR_Category",
        style="WAR_Category",
        markers={"Mid WAR (1.5-3.0)": "o", "Top WAR (4.0+)": "s"},
        dashes=False,
        linewidth=2.5,
        markersize=8,
        palette={"Mid WAR (1.5-3.0)": "firebrick", "Top WAR (4.0+)": "tab:blue"},
        ax=ax,
    )

    # Add regression lines using seaborn's regplot
    for category, color, alpha_color in [
        ("Mid WAR (1.5-3.0)", "firebrick", "salmon"),
        ("Top WAR (4.0+)", "tab:blue", "tab:cyan"),
    ]:
        category_data = plot_data[plot_data["WAR_Category"] == category]
        sns.regplot(
            data=category_data,
            x="Season",
            y="Percentage",
            scatter=False,
            line_kws={"linestyle": "--", "alpha": 0.7, "linewidth": 1.5},
            color=alpha_color,
            ax=ax,
        )

    # Customize plot
    ax.set_xlabel("Season", fontsize=12)
    ax.set_ylabel("% of Pitchers in Bucket", fontsize=12)
    ax.set_title("Mid vs Top WAR Percentage by Season", fontsize=14, fontweight="bold")

    # Format y-axis as percentages
    ax.yaxis.set_major_formatter(PercentFormatter(xmax=1.0, decimals=0))

    # Force integer x-axis ticks
    ax.set_xticks(compiled_stats["Season"])

    # Improve legend
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(
        handles[:2],  # Only show the main line labels, not regression
        labels[:2],
        title="WAR Category",
        loc="best",
        frameon=True,
        shadow=True,
    )

    # Add value annotations
    for _, row in compiled_stats.iterrows():
        ax.annotate(
            f"{row['mid_war_pct']:.1%}",
            xy=(row["Season"], row["mid_war_pct"]),
            xytext=(0, -15),
            textcoords="offset points",
            ha="center",
            fontsize=7,
            color="black",
            alpha=0.8,
        )

        ax.annotate(
            f"{row['top_war_pct']:.1%}",
            xy=(row["Season"], row["top_war_pct"]),
            xytext=(0, -15),
            textcoords="offset points",
            ha="center",
            fontsize=7,
            color="black",
            alpha=0.8,
        )

    plt.tight_layout()
    plt.show()

    markdown_table = compiled_stats.drop(columns=["top_war_pct", "mid_war_pct"])
    print(markdown_table.to_markdown())


if __name__ == "__main__":
    main()
