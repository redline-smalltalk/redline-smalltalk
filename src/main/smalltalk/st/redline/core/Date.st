"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

Magnitude subclass: #Date.

"category: general inquiries"

- dayOfWeek: dayName
    "Answer the index in a week, 1, 2 .... or 7, of the day named as the argument, dayName."
    self notImplementedYet.

- nameOfDay: dayIndex
    "Answer a Symbol that represents the name of the day whose index is the argument, dayIndex, where 1 is Monday,
     2, is Tuesday, and so on."
    self notImplementedYet.

- indexOfMonth: monthName
    "Answer the index in a year, 1, 2 .... or 12, of the month named as the argument, monthName."
    self notImplementedYet.

- nameOfMonth: monthIndex
    "Answer a Symbol that represents the name of the month whose index is the argument, monthIndex, where 1 is January,
     2, is February, and so on."
    self notImplementedYet.

- daysInMonth: monthName forYear: yearInteger
    "Answer the number of days in the month whose name is monthName in the year yearlnteger (the year must be known in
     order to account for a leap year)."
    self notImplementedYet.

- daysInYear: yearInteger
    "Answer the number of days in the year, yearInteger."
    self notImplementedYet.

- leapYear: yearInteger
    "Answer 1 if the year yearInteger is a leap year; answer 0 otherwise."
    self notImplementedYet.

- dateAndTimeNow
    "Answer an Array whose first element is the current date (an instance of class Date representing today's date) and
     whose second element is the current time (an instance of class Time representing the time right now)."
    self notImplementedYet.


"category: instance creation"

+ today
    "Answer an instance of Date representing the day the message is sent."
    self notImplementedYet.

+ fromDays: dayCount
    "Answer an instance of Date that is dayCount number of days before or after January 1, 1901 (depending on the sign
     of the argument)."
    self notImplementedYet.

+ newDay: day month: monthName year: yearInteger
    "Answer an instance of Date that is day number of days into the month named monthName in the year yearInteger."
    self notImplementedYet.

+ newDay: dayCount year: yearInteger
    "Answer an instance of Date that is dayCount number of days after the beginning of the year yearInteger."
    self notImplementedYet.

"category: arithmetic"

- addDays: dayCount
    "Answer a Date that is dayCount number of days after the receiver."
    self notImplementedYet.

- subtractDays: dayCount
    "Answer a Date that is dayCount number of days before the receiver."
    self notImplementedYet.

- subtractDate: aDate
    "Answer an Integer that represents the number of days between the receiver and the argument, aDate."
    self notImplementedYet.

"category: converting"

- asSeconds
    "Answer the number of seconds between a time on January 1, 1901, and the same time in the receiver's day."
    self notImplementedYet.
