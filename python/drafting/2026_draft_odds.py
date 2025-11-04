import numpy as np
import pprint

"""                                                    
     ▄▄  ▄▄  ▄▄  ▗▄     ▗  ▖▗   ▗▄▄     ▗▄▖          ▗▀  ▗  
    ▝ ▝▌▗▘▝▖▝ ▝▌▗▘ ▘    ▐▌▐▌▐   ▐  ▌    ▐ ▝▖ ▖▄  ▄▖ ▗▟▄ ▗▟▄ 
      ▗▘▐ ▖▌  ▗▘▐▞▜▖    ▐▐▌▌▐   ▐▄▄▘    ▐  ▌ ▛ ▘▝ ▐  ▐   ▐  
     ▗▘ ▐  ▌ ▗▘ ▐  ▌    ▐▝▘▌▐   ▐  ▌    ▐  ▌ ▌  ▗▀▜  ▐   ▐  
    ▗▙▄▖ ▙▟ ▗▙▄▖ ▙▟▘    ▐  ▌▐▄▄▖▐▄▄▘    ▐▄▞  ▌  ▝▄▜  ▐   ▝▄ 

    A script to estimate the Cardinals' 2026 MLB Draft position odds by
    simulating the draft.

"""

NUM_SIMULATIONS = 1_000_000


def set_up_data():
    """
        I copied the odds of each team in the draft lottery getting the #1 pick from
        MLB.com: https://www.mlb.com/news/odds-for-2026-mlb-draft-lottery

        White Sox (.370) -- 27.73%
        Twins (.432) -- 22.18%
        Pirates (.438) -- 16.81%
        Orioles (.463) -- 9.24%
        A’s (.469) -- 6.55%
        Braves (.469) -- 4.54%
        Rays (.475) -- 3.03%
        Cardinals (.481) -- 2.35%
        Marlins (.488) -- 1.85%
        D-backs (.494) -- 1.51%
        Rangers (.500) -- 1.34%
        Giants (.500) -- 1.01%
        Royals (.506) -- 0.84%
        Mets (.512) -- 0.67%
        Astros (.537) -- 0.34%
        Rockies (.265) -- ineligible
        Nationals (.407) -- ineligible
        Angels (.444) -- ineligible
    """
    record_order = np.array([
        "COL", "CHW", "WAS", "MIN", "PIT", "LAA",
        "BAL", "ATH", "ATL", "TBR", "STL", "MIA",
        "ARI", "TEX", "SFG", "KCR", "NYM", "HOU",
    ])

    ineligible = np.array([ "COL", "WAS", "LAA", ])

    lottery = np.concatenate(
        [np.full(2773, "CHW"), np.full(2118, "MIN"), np.full(1681, "PIT"),
         np.full(924, "BAL"), np.full(655, "ATH"), np.full(454, "ATL"),
         np.full(303, "TBR"), np.full(235, "STL"), np.full(185, "MIA"),
         np.full(151, "ARI"), np.full(134, "TEX"), np.full(101, "SFG"),
         np.full(84, "KCR"), np.full(67, "NYM"), np.full(34, "HOU"),]
    )

    return lottery, record_order, ineligible

def draft(lottery, record_order, ineligible):
    """
        As I understand the rules for selecting draft order, the process is:
         1. All eligible teams are entered in the lottery with the given odds.
         2. Once a team is chosen, their "tickets" are removed from the lottery,
            rather than redistributed.
         3. After the top six lottery spots are chosen, the remaining teams are
            placed in order of their regular season record.
            a. Lottery-ineligible teams can't pick less than 10th, so if they
               would land at a non-lottery spot less than 10 (that is, 7-9th)
               they're placed at #10 and beyond, according to their regular
               season record.
            b. Lottery eligibility is based on revenue sharing status and
               previous years positions within the lottery. I haven't
               attempted to reproduce that logic, I'm just taking MLB's
               word for it. They should know.
    """
    draft_order = np.array([])

    # the lottery for the first six spots
    for _ in range(6):
        lottery, record_order, draft_order = pick(draft_order, lottery, record_order)

    # Round out the top-9, since ineligible teams aren't allowed to pick under 10
    eligible_top_nine_teams_picked = 0
    for team in record_order:
        if team not in ineligible:
            lottery, record_order, draft_order = pick(draft_order, lottery, record_order)
            eligible_top_nine_teams_picked += 1

        if eligible_top_nine_teams_picked > 2:
            break

    draft_order = np.concatenate([draft_order, record_order])
    return draft_order


def pick(draft_order, lottery, record_order):
    choice = np.random.choice(lottery)
    draft_order = np.append(draft_order, choice)
    lottery = lottery[lottery != choice]
    record_order = record_order[record_order != choice]
    return lottery, record_order, draft_order

def main():
    cardinals_draft_spots = np.array([])
    lottery, record_order, ineligible_teams = set_up_data()

    for _ in range(NUM_SIMULATIONS):
        # Make copies since these records will be modified
        lottery_copy = np.copy(lottery)
        record_copy = np.copy(record_order)
        order = draft(lottery_copy, record_copy, ineligible_teams)
        cardinals_spot = np.where(order == "STL")[0]
        # Don't forget, arrays are zero-indexed but lotteries aren't!
        cardinals_draft_spots = np.append(cardinals_draft_spots, cardinals_spot + 1)

    cardinals_draft_spots = np.array(cardinals_draft_spots)
    draft_spot_values, draft_spot_counts = np.unique(cardinals_draft_spots, return_counts=True)
    raw_draft_result = {int(k): int(v) for k, v in zip(draft_spot_values, draft_spot_counts)}
    percent_draft_result = {}

    for key in raw_draft_result:
        percentage = raw_draft_result[key] / NUM_SIMULATIONS
        percent_draft_result[key] = f"{percentage:.5%}"

    pprint.pprint(percent_draft_result)

if __name__ == "__main__":
    main()