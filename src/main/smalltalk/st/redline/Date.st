" Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution "

Magnitude < #Date
"general inquiries"
+ dayOfWeek: dayName
    self subclassResponsibility.

+ nameOfDay: dayIndex
    self subclassResponsibility.

+ indexOfMonth: monthName
    self subclassResponsibility.

+ nameOfMonth: monthIndex
    self subclassResponsibility.

+ daysInMonth: monthName forYear: yearInteger
    self subclassResponsibility.

+ daysInYear: yearInteger
    self subclassResponsibility.

+ leapYear: yearInteger
    self subclassResponsibility.

+ dateAndTimeNow
    self subclassResponsibility.

