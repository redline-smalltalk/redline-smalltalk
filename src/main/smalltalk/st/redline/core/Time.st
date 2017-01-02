"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Magnitude subclass: #Time.

"category: general inquiries"

- millisecondClockValue
    "Answer the number of milliseconds since the millisecond clock was last reset or rolled over to 0."
    self notImplementedYet.

- millisecondsToRun: timedBlock
    "Answer the number of milliseconds timedBlock takes to return its value."
    self notImplementedYet.

- timeWords
    "Answer the seconds (in Greenwich Mean Time) since Jan. 1, 1901."
    self notImplementedYet.

- totalSeconds
    "Answer the total seconds from Jan. 1, 1901, corrected for time zone and daylight savings time."
    self notImplementedYet.

- dateAndTimeNow
    "Answer an Array whose first element is the current date (an instance of class Date that represents today's date)
     and whose second element is the current time (an instance of class Time that represents the time right now). The
     result of sending this message to Time is identical to the result of sending it to Date."
    self notImplementedYet.

"category: instance creation"

+ now
    "Answer an instance of Time representing the second the message is sent."
    self notImplementedYet.

+ fromSeconds: secondCount
    "Answer an instance of Time that is secondCount number of seconds since midnight."
    self notImplementedYet.

"category: arithmetic"

- addTime: timeAmount
    "Answer an instance of Time that is the argument, timeAmount, after the receiver."
    self notImplementedYet.

- subtractTime: timeAmount
    "Answer an instance of Time that is the argument, timeAmount, before the receiver."
    self notImplementedYet.

"category: converting"

- asSeconds
    "Answer the number of seconds since midnight that the receiver represents."
    self notImplementedYet.
