:- module(problems, [problem/2]).

problem(vakantiedagen, [
            "If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days",
            "If the years of service of an employee is at least 30, then he receives 5 extra days",
            "If years of service of an employee is at least 30 then he receives 3 extra days",
            "If age of an employee is at least 60 then he receives 3 extra days",
            "If the years of service of an employee is between 15 and 30 and he does not receive 5 extra days then he receives 2 extra days",
            "If the age of an employee is at least 45 and he does not receive 5 extra days then he receives 2 extra days",
            "Each employee has a number of vacation days equal to 22 plus the sum of the extra days he receives"
        ]).


%?- parse_tree("If the age of an employee is less than 18 or greather than 60 then he receives 5 extra days", s).

problem(zebra, [
            "The Englishman lives in the red house",
            "The Spaniard keeps the dog",
            "There is a person who lives in the green house and drinks coffee",
            "The Ukrainian drinks tea",
            "The position of the green house is equal to the position of the ivory house plus 1",
            "There is a person who smokes Old Gold and keeps the snail",
            "There is a person who lives in a yellow house and smokes Kools",
            "The person who lives in the third house, drinks milk",
            "The Norwegian lives in the first house",
            "A house A is next to a house B if the absolute value of the difference between the position of house A and the position of house B is equal to 1",
            "The house in which the person who smokes Chesterfields lives, is next to the house in which the person who keeps the fox lives",
            "The house in which the person who smokes Kools lives, is next to the house in which the person who keeps the horse lives",
            "The person who smokes Lucky Strike, drinks Orange Juice",
            "The Japanese smokes Parliaments",
            "The Norwegian lives in a house that is next to the blue house",
            "Every animal is kept by exactly 1 Person"
      ]).

problem(translators, [
            "The Englishman speaks German.",
            "The Spaniard speaks French.",
            "The German does not speak Spanish.",
            "The Frenchman does not speak German.",
            "Spaniard and Englishman both speak German.",
            "Englishman and Frenchman both speak two languages.",
            "Four translators speak Russian.",
            "Exactly two translators speak French.",
            "Only one translator who speaks Spanish, speaks Russian.",
            "Russian and Englishman have no common languages except their native languages."
        ]).

problem(thieves, [
            "The hacker and the overlooker both do not attend to the meeting.",
            "Bob does not know the accessory after the fact",
            "Charley knows the driver",
            "Damian knows only one of the thieves",
            "Ernest knows three of the thieves",
            "Albert knows only two of the thieves",
            "the hacker is the only one which knows only one other",
            "the overlooker knows three other thieves."
        ]).

problem(swimming_suits, [
            "The last name of Rachel is Travers.",
            "Rachel does not wear a red bathing suit.",
            "Rachel places one higher than the contestant wearing the white 1-piece bathing suit",
            "The last name of Melony was not James.",
            "Melony won first place.",
            "The only three who wear 1-piece suites, are the contestant in second place, the contestant who wears the yellow bathing suit and Amelia.",
            "The last name of Amelia is not West.",
            "Rachel places one higher than the contestant whose last name is Couch.",
            "Rachel places two lower than the contestant who wore the blue bathing suit.",
            "Julia's last name was not Couch.",
            "James wears a 2-piece bathing suit.",
            "The five contestants were James, Julia, the contestant who placed fifth, the contestant who wears the black 2-piece bathing suit and West"
        ]).

problem(extra, [
            "There is a blue house or every person who smokes Lucky Strike, drinks Orange Juice",
            "There is a blue house in which the Norwegian lives or every person who smokes Lucky Strike drinks Orange Juice",
            "Every blue house is next to a red house or is next to every green house",
            "Every blue house is next to every green house or is next to a red house",
            %"Every blue house is next to a red house in which the person who drinks tea lives or is next to the green house",
            "Every blue house is next to the green house or is next to a red house in which the person who drinks tea lives",
            "There is a person who lives in the green house",
            "Every person who drinks tea lives in a blue house",
            "There is a person who lives in the green house and drinks coffee and keeps the snail",
            "There is a person who lives in the green house and lives in a red house and lives in every blue house",
            "There is a house in which the Norwegian lives and the Ukrainian lives and the Spaniard lives",
            "The person who lives in the green house or lives in the red house, lives in the blue house"
        ]).