import copy
import numpy as np

"""
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


def draft(lottery, record_order):
    draft_order = []
    # the lottery for the first six spots
    for _ in range(6):
        choice = np.random.choice(lottery)
        # print(f"{choice} was drafted")
        draft_order.append(choice)
        lottery = lottery[lottery != choice]
        record_order.remove(choice)
        # print(record_order)

    draft_order = np.concatenate([draft_order, record_order])
    return draft_order

if __name__ == "__main__":
    record_order = [
        "COL",
        "CHW",
        "WAS",
        "MIN",
        "PIT",
        "LAA",
        "BAL",
        "ATH",
        "ATL",
        "TBR",
        "STL",
        "MIA",
        "ARI",
        "TEX",
        "SFG",
        "KCR",
        "NYM",
        "HOU",
    ]

    lottery = np.concatenate(
        [
            np.full(2773, "CHW"),
            np.full(2118, "MIN"),
            np.full(1681, "PIT"),
            np.full(924, "BAL"),
            np.full(655, "ATH"),
            np.full(454, "ATL"),
            np.full(303, "TBR"),
            np.full(235, "STL"),
            np.full(185, "MIA"),
            np.full(151, "ARI"),
            np.full(134, "TEX"),
            np.full(101, "SFG"),
            np.full(84, "KCR"),
            np.full(67, "NYM"),
            np.full(34, "HOU"),
        ]
    )

    spots = []

    ONE_HUNDRED_K = 100000

    for _ in range(ONE_HUNDRED_K):
        i_lottery = copy.deepcopy(lottery)
        i_record_order = copy.deepcopy(record_order)
        order = draft(i_lottery, i_record_order)
        cardinals_draft_spot = np.where(order == "STL")
        spots.append(cardinals_draft_spot)
        print(cardinals_draft_spot)
        break

    spots = np.array(spots)
    values, counts = np.unique(spots, return_counts=True)
    raw_result = {int(k): int(v) for k, v in zip(values, counts)}
    print(raw_result)
    percent_result = {}
    for key in raw_result:
        percent_result[key] = raw_result[key] / ONE_HUNDRED_K


    print(percent_result)