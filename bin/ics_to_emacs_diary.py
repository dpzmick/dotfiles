#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.11"
# dependencies = [
#   "icalendar",
#   "python-dateutil",
#   "recurring-ical-events",
# ]
# ///

from __future__ import annotations

import argparse
from datetime import datetime, timedelta, date, time
from pathlib import Path
from urllib.request import Request, urlopen

import recurring_ical_events
from dateutil.tz import gettz
from icalendar import Calendar


def fetch_ics(url: str) -> bytes:
    # Fastmail ICS feeds can be behind long URLs; set a UA to be polite.
    req = Request(url, headers={"User-Agent": "fastmail-ics-to-diary/1.0"})
    with urlopen(req, timeout=30) as resp:
        return resp.read()


def to_local_dt(d, tz):
    if isinstance(d, datetime):
        if d.tzinfo is None:
            return d.replace(tzinfo=tz)
        return d.astimezone(tz)
    if isinstance(d, date):
        return datetime.combine(d, time.min).replace(tzinfo=tz)
    raise TypeError(f"Unexpected dt type: {type(d)}")


def fmt_diary_line(start_local: datetime, summary: str, all_day: bool) -> str:
    d = start_local.strftime("%m/%d/%Y")
    if all_day:
        return f"{d} {summary}"
    t = start_local.strftime("%H:%M")
    return f"{d} {t} {summary}"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--url", required=True, help="Fastmail ICS feed URL")
    ap.add_argument("--out", default="~/.emacs.d/fastmail-diary", help="Output diary file")
    ap.add_argument("--days", type=int, default=14, help="How many days ahead to include")
    ap.add_argument("--tz", default="America/Chicago", help="IANA timezone name")
    args = ap.parse_args()

    tz = gettz(args.tz)
    if tz is None:
        raise SystemExit(f"Unknown timezone: {args.tz}")

    out_path = Path(args.out).expanduser()

    ics_bytes = fetch_ics(args.url)
    cal = Calendar.from_ical(ics_bytes)

    now = datetime.now(tz)
    start = now
    end = (now + timedelta(days=args.days)).replace(hour=23, minute=59, second=59, microsecond=0)

    occurrences = recurring_ical_events.of(cal).between(start, end)

    rows = []
    for ev in occurrences:
        summary = str(ev.get("SUMMARY", "Untitled")).replace("\n", " ").strip()
        dtstart = ev.get("DTSTART")
        if not dtstart:
            continue

        raw_start = dtstart.dt
        all_day = isinstance(raw_start, date) and not isinstance(raw_start, datetime)
        start_local = to_local_dt(raw_start, tz)

        if start_local < start or start_local > end:
            continue

        rows.append((start_local, fmt_diary_line(start_local, summary, all_day)))

    rows.sort(key=lambda x: x[0])

    header = f";; Auto-generated from Fastmail ICS (next {args.days} days). Do not edit.\n"
    content = header + "\n".join(line for _, line in rows) + ("\n" if rows else "")

    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(content, encoding="utf-8")


if __name__ == "__main__":
    main()
