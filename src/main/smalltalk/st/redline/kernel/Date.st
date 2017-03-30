"Redline Smalltalk, Copyright (c) James C. Ladd. All rights reserved. See LICENSE in the root of this distribution."

"I represent a date. My printing format consists of an array of six elements.

The first three elements contain the numbers 1, 2, 3, in any order. 1 indicates that the day appears in this position, 2 indicates that the month appears in this position, and 3 indicates that the year appears in this position.

The fourth element is the ascii value of the character separator or the character itself.

The fifth element is the month format, where 1 indicates print as a number, 2 indicates print the first three characters, and 3 indicates print the entire name.

The six element is the year format, where 1 indicates print as a number, and 2 indicates print the number modulo 100.

Examples:
	#(1 2 3 32 2 1) prints as 12 Dec 1981
	#(2 1 3 $/ 1 2) prints as 12/12/81'
"

Magnitude subclass: #Date
          instanceVariableNames: 'day year '
          classVariableNames: 'DaysInMonth FirstDayOfMonth MonthNames SecondsInDay WeekDayNames '
          poolDictionaries: ''
          category: 'Numeric-Magnitudes'.

"Date methodsFor: 'accessing'"

- day
    "Answer the day of the year represented by the receiver."

    ^ day.

- leap
    "Answer whether the receiver's year is a leap year."

    ^ Date leapYear: year.

- monthIndex
    "Answer the index of the month in which the receiver falls."

    | leap firstDay |
    leap := self leap.
    12 to: 1 by: -1 do:
      [ :monthIndex |
        firstDay := (FirstDayOfMonth at: monthIndex)
                + (monthIndex > 2 ifTrue: [leap] ifFalse: [0]).
        firstDay <= day
          ifTrue: [^ monthIndex]].
    self error: 'illegal month'.

- monthName
    "Answer the name of the month in which the receiver falls."

    ^ MonthNames at: self monthIndex.

- weekday
    "Answer the name of the day of the week on which the receiver falls."

    ^ WeekDayNames at: self weekdayIndex.

- year
    "Answer the year in which the receiver falls."

    ^ year.


"Date methodsFor: 'arithmetic'"

- addDays: dayCount
    "Answer a Date that is dayCount days after the receiver."

    ^ Date newDay: day + dayCount
        year: year.

- subtractDate: aDate
    "Answer the number of days between the receiver and aDate."

    year = aDate year
      ifTrue: [^ day - aDate day]
      ifFalse: [^ year - 1 // 4 - (aDate year // 4) + day
              + aDate daysLeftInYear + (year - 1 - aDate year * 365)].

- subtractDays: dayCount
    "Answer a Date that is dayCount days before the receiver."

    ^ Date newDay: day - dayCount year: year.


"Date methodsFor: 'comparing'"

- < aDate
    "Answer whether aDate precedes the date of the receiver."

    year = aDate year
      ifTrue: [^ day < aDate day]
      ifFalse: [^ year < aDate year].

- = aDate
    "Answer whether aDate is the same day as the receiver."

    self species = aDate species
      ifTrue: [^ day = aDate day & (year = aDate year)]
      ifFalse: [^ false].

- hash
    "Hash is reimplemented because = is implemented."

    ^ (year hash bitShift: 3) bitXor: day.


"Date methodsFor: 'inquiries'"

dayOfMonth
	"Answer which day of the month is represented by the receiver."

	^day - (self firstDayOfMonthIndex: self monthIndex) + 1! !

!Date methodsFor: 'inquiries'!
- daysInMonth
    "Answer the number of days in the month represented by the receiver."

    ^ (DaysInMonth at: self monthIndex)
      + (self monthIndex = 2
          ifTrue: [self leap]
          ifFalse: [0]).

- daysInYear
    "Answer the number of days in the year represented by the receiver."

    ^ Date daysInYear: self year.

- daysLeftInYear
    "Answer the number of days in the year after the date of the receiver."

    ^ self daysInYear - self day.

- firstDayOfMonth
    "Answer the index of the day of the year that is the first day of the
    receiver's month."

    ^ self firstDayOfMonthIndex: self monthIndex.

- previous: dayName
    "Answer the previous date whose weekday name is dayName."

    ^ self subtractDays: 7 + self weekdayIndex - (Date dayOfWeek: dayName) \\ 7.

"Date methodsFor: 'converting'"

- asSeconds
    "Answer the seconds between a time on 1 January 1901 and the same
    time in the receiver's day."

    ^ SecondsInDay * (self subtractDate: (Date newDay: 1 year: 1901)).


"Date methodsFor: 'printing'"

- mmddyy
    "Please use mmddyyyy instead, so dates in 2000 will be unambiguous"
    "Answer the receiver rendered in standard fmt mm/dd/yy. 1/17/96 sw.
    2/1/96 sw Fixed to show day of month, not day.
    Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, so that for
    example feb 1 1996 is 2/1/96"

    "Date today mmddyy"

    ^ self printFormat: #(2 1 3 $/ 1 2).

- mmddyyyy
    "Answer the receiver rendered in standard fmt mm/dd/yyyy.  Good for avoiding year 2000 bugs.
    Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, so that for
    example feb 1 1996 is 2/1/96"

    "Date today mmddyyyy"

    ^ self printFormat: #(2 1 3 $/ 1 1).

- printFormat: formatArray
    "Answer a String describing the receiver using the format denoted by the
    argument, formatArray."

    | aStream |
    aStream := WriteStream on: (String new: 16).
    self printOn: aStream format: formatArray.
    ^ aStream contents.

- printOn: aStream

    self printOn: aStream format: #(1 2 3 $  3 1 ).

- printOn: aStream format: formatArray
    "Print a description of the receiver on aStream using the format denoted
    by the argument, formatArray:
      #(item item item sep monthfmt yearfmt twoDigits)
      items:  1=day  2=month  3=year  will appear in the order given,
      separated by sep which is eaither an ascii code or character.
      monthFmt:  1=09  2=Sep  3=September
      yearFmt:  1=1996  2=96
      digits:  (missing or)1=9  2=09.
    See the examples in printOn: and mmddyy"
    | monthIndex element monthFormat twoDigits monthDay |
    twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
    monthIndex := self monthIndex.
    1 to: 3 do:
      [:elementIndex |
      element := formatArray at: elementIndex.
      element = 1 ifTrue:
        [monthDay := day - self firstDayOfMonth + 1.
        twoDigits & (monthDay < 10) ifTrue: [aStream nextPutAll: '0'].
          monthDay printOn: aStream].
      element = 2 ifTrue:
        [monthFormat := formatArray at: 5.
        monthFormat = 1 ifTrue:
          [twoDigits & (monthIndex < 10) ifTrue: [aStream nextPutAll: '0'].
          monthIndex printOn: aStream].
        monthFormat = 2 ifTrue:
          [aStream nextPutAll: ((MonthNames at: monthIndex)
                          copyFrom: 1 to: 3)].
        monthFormat = 3 ifTrue:
          [aStream nextPutAll: (MonthNames at: monthIndex)]].
      element = 3 ifTrue:
        [(formatArray at: 6) = 1
          ifTrue: [year printOn: aStream]
          ifFalse: [twoDigits & ((year \\ 100) < 10)
                ifTrue: [aStream nextPutAll: '0'].
              (year \\ 100) printOn: aStream]].
      elementIndex < 3 ifTrue:
        [(formatArray at: 4) ~= 0
          ifTrue: [aStream nextPut: (formatArray at: 4) asCharacter]]].

- storeOn: aStream

      aStream nextPutAll: '(' + self class name + ' readFromString: ';
        print: self printString;
        nextPut: $).


"Date methodsFor: 'private'"

- day: dayInteger year: yearInteger

    day := dayInteger.
    year := yearInteger.

- firstDayOfMonthIndex: monthIndex
    "Answer the day of the year (an Integer) that is the first day of my month"

    ^ (FirstDayOfMonth at: monthIndex)
      + (monthIndex > 2
          ifTrue: [self leap]
          ifFalse: [0]).

- weekdayIndex
    "Monday=1, ... , Sunday=7"

    | yearIndex dayIndex |
    day < (self firstDayOfMonthIndex: 3)
      ifTrue:
        [yearIndex := year - 1.
        dayIndex := 307]
      ifFalse:
        [yearIndex := year.
        dayIndex := -58 - self leap].

    ^ dayIndex + day + yearIndex + (yearIndex // 4)
          + (yearIndex // 400) - (yearIndex // 100) \\ 7 + 1.

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "


"Date class methodsFor: 'class initialization'"

+ initialize
    "Initialize class variables representing the names of the months and days and
    the number of seconds, days in each month, and first day of each month."

    MonthNames :=
      #(January February March April May June
        July August September October November December ).
    SecondsInDay := 24 * 60 * 60.
    DaysInMonth := #(31 28 31 30 31 30 31 31 30 31 30 31 ).
    FirstDayOfMonth := #(1 32 60 91 121 152 182 213 244 274 305 335 ).
    WeekDayNames :=
      #(Monday Tuesday Wednesday Thursday Friday Saturday Sunday ).


"Date class methodsFor: 'instance creation'"

+ fromDays: dayCount
    "Answer an instance of me which is dayCount days after January 1,
    1901."

    ^ self
      newDay: 1 + (dayCount asInteger rem: 1461)
                "There are 1461 days in a 4-year cycle.
                 2000 is a leap year, so no extra correction is necessary. "
      year: 1901 + ((dayCount asInteger quo: 1461) * 4).

+ fromString: aString
    "Answer an instance of created from a string with format DD.MM.YYYY."

    | fields |
    fields := aString findTokens: './'.
    ^ self newDay: (fields at: 1) asNumber month: (fields at: 2) asNumber year: (fields at: 3) asNumber.

+ newDay: day month: monthName year: year
    "Answer an instance of me which is the day'th day of the month named
     monthName in the year'th year. The year may be specified as the actual
     number of years since the beginning of the Roman calendar or the
     number of years since 1900.  **Note** two digit dates are always from 1900.
      1/1/01 will NOT mean 2001."
    "Tolerate a month index instead of a month name."

    | monthIndex daysInMonth firstDayOfMonth |
    year < 100 ifTrue: [^ self
        newDay: day
        month: monthName
        year: 1900 + year].
    monthIndex := monthName isInteger
     ifTrue: [monthName] ifFalse: [self indexOfMonth: monthName].
    monthIndex = 2
      ifTrue: [daysInMonth := (DaysInMonth at: monthIndex)
              + (self leapYear: year)]
      ifFalse: [daysInMonth := DaysInMonth at: monthIndex].
    monthIndex > 2
      ifTrue: [firstDayOfMonth := (FirstDayOfMonth at: monthIndex)
              + (self leapYear: year)]
      ifFalse: [firstDayOfMonth := FirstDayOfMonth at: monthIndex].
    (day < 1 or: [day > daysInMonth])
      ifTrue: [self error: 'illegal day in month']
      ifFalse: [^ self new day: day - 1 + firstDayOfMonth year: year].

+ newDay: dayCount year: referenceYear
    "Answer an instance of me which is dayCount days after the beginning
    of the year referenceYear."

    | day year daysInYear |
    day := dayCount.
    year := referenceYear.
    [day > (daysInYear := self daysInYear: year)]
      whileTrue:
        [year := year + 1.
         day := day - daysInYear].
    [day <= 0]
      whileTrue:
        [year := year - 1.
         day := day + (self daysInYear: year)].
    ^ self new day: day year: year.

+ readFrom: aStream
    "Read a Date from the stream in any of the forms:
      <day> <monthName> <year>		(5 April 1982; 5-APR-82)
      <monthName> <day> <year>		(April 5, 1982)
      <monthNumber> <day> <year>	(4/5/82)"

    | day month |
    aStream peek isDigit ifTrue: [day := Integer readFrom: aStream].
    [aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
    aStream peek isLetter
      ifTrue:		"number/name... or name..."
        [month := WriteStream on: (String new: 10).
        [aStream peek isLetter] whileTrue: [month nextPut: aStream next].
        month := month contents.
        day isNil ifTrue:		"name/number..."
          [[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
          day := Integer readFrom: aStream]]
      ifFalse:		"number/number..."
        [month := Date nameOfMonth: day.
        day := Integer readFrom: aStream].
    [aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
    ^ self newDay: day month: month year: (Integer readFrom: aStream).

+ today
    "Answer an instance of me representing the day and year right now."

    ^ self dateAndTimeNow at: 1.


"Date class methodsFor: 'general inquiries'"

+ dateAndTimeNow
    "Answer an Array whose first element is Date today and second element
    is Time now."

    ^ Time dateAndTimeNow.

+ dayOfWeek: dayName
    "Answer the index in a week, 1-7, of the day named dayName. Create an
    error notification if no such day exists."

    1 to: 7 do: [:index | (WeekDayNames at: index)
        = dayName ifTrue: [^index]].
    self error: dayName asString + ' is not a day of the week'.

+ daysInMonth: monthName forYear: yearInteger
    "Answer the number of days in the month named monthName in the
    year yearInteger."

    ^ (self newDay: 1
        month: monthName
        year: yearInteger) daysInMonth.

+ daysInYear: yearInteger
    "Answer the number of days in the year, yearInteger."

    ^365 + (self leapYear: yearInteger).

+ firstWeekdayOfMonth: mn year: yr
    "Answer the weekday index (Sunday=1, etc) of the first day in the month named mn in the year yr."

    ^ (self newDay: 1 month: mn year: yr) weekdayIndex + 7 \\ 7 + 1.

+ indexOfMonth: monthName
    "Answer the index, 1-12, of the month monthName. Create an error
    notification if no such month exists."

    1 to: 12 do:
      [ :index |
        (monthName , '*' match: (MonthNames at: index))
              ifTrue: [^index]].
    self error: monthName + ' is not a recognized month name'.

+ leapYear: yearInteger
    "Answer 1 if the year yearInteger is a leap year; answer 0 if it is not."

    (yearInteger \\ 4 ~= 0 or: [yearInteger \\ 100 = 0 and: [yearInteger \\ 400 ~= 0]])
      ifTrue: [^ 0]
      ifFalse: [^ 1].

+ nameOfDay: dayIndex
    "Answer a symbol representing the name of the day indexed by
    dayIndex, 1-7."

    ^ WeekDayNames at: dayIndex.

+ nameOfMonth: monthIndex
    "Answer a String representing the name of the month indexed by
    monthIndex, 1-12."

    ^ MonthNames at: monthIndex.
