(*  Assignment #1 CSC 330 *)

type DATE = (int * int * int)
exception InvalidParameter

(* Return whether the first date comes before the second *)
fun is_older(d1: DATE, d2: DATE): bool =
    if #1 d1 < #1 d2 
        then true
    else if #1 d1 > #1 d2 
        then false
    else if #2 d1 < #2 d2 (* guaranteed that the years match after this point *) 
        then true 
    else if #2 d1 > #2 d2 
        then true
    else (* guaranteed that the months match *)
        #3 d1 < #3 d2 

(* Compute the number of dates in the list which have a month matching the one given *)
fun number_in_month(dates: DATE list, month: int): int =
    if null dates then 0
    else 
        let
            val number_in_tail = number_in_month(tl dates, month)
        in
            if (#2 (hd dates)) = month 
                then 1 + number_in_tail
            else number_in_tail
        end

(* Compute the number of dates in the list which have a month matching any of those given *)
fun number_in_months(dates: DATE list, months: int list): int =
    if null dates orelse null months 
        then 0
    else 
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Get the sub-list of dates passed in which match the given month *)
fun dates_in_month(dates: DATE list, month: int): DATE list =
    if null dates 
        then []
    else
        let
            val matching_dates_in_tail = dates_in_month(tl dates, month)
        in
            if (#2 (hd dates)) = month 
                then (hd dates)::matching_dates_in_tail
            else matching_dates_in_tail
        end

(* Get the sub-list of dates passed in which match any of the given months *)
fun dates_in_months(dates: DATE list, months: int list): DATE list =
    if null months orelse null dates 
        then []
    else
        let
            val dates_in_tail_months = dates_in_months(dates, tl months)
            val dates_in_head_month = dates_in_month(dates, hd months)
        in
            dates_in_head_month@dates_in_tail_months
        end

(* Gets the nth string in the given list, where the first string is at index 1 *)
fun get_nth(strings: string list, n: int): string =
    if n = 0 orelse null strings 
        then raise InvalidParameter
    else if n = 1 
        then hd strings
    else
        get_nth(tl strings, n-1)

(* Return a string representing the given date in worsds *)
fun date_to_string(date: DATE): string =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val monthStr = get_nth(months, #2 date)
        val dayStr = Int.toString(#3 date)
        val yearStr = Int.toString(#1 date)
    in
        monthStr ^ " " ^ dayStr ^ ", " ^ yearStr 
    end

(* 
 * Given a positive value sum and a list of positive values, determine how many items 
 * in the list can be added together before reaching the sum value 
 *)
fun number_before_reaching_sum(sum: int, nums: int list): int = 
    if null nums orelse sum <= hd(nums) 
        then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums )

(* Given a day of the year, determine what month it is in *)
fun what_month(day_of_year: int): int =
    let
        val month_lengths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, month_lengths) + 1
    end

(*
 * Given a range specified using two days of the year, return a list of the 
 * months in which each day in the range is in
 *)
fun month_range(d1: int, d2: int): int list =
    if d1 > d2 
        then [] 
    else 
        what_month(d1)::month_range(d1 + 1, d2)

(* Return the oldest date in the list as an option. NONE given for empty list. *)
fun oldest(dates: DATE list): DATE option =
    if null dates 
        then NONE
    else 
        let 
            fun oldest_non_empty(dates: DATE list): DATE =
                if null (tl dates)
                    then hd dates
                else 
                    let 
                        val oldest_in_tail = oldest_non_empty(tl dates) 
                    in 
                        if is_older(hd dates, oldest_in_tail) 
                            then hd dates
                        else 
                            oldest_in_tail
                    end
        in 
            (* We are guaranteed that the list is non-empty, so we will get a result from this call *)
            SOME (oldest_non_empty(dates))
        end

(* Determines whether the given date makes sense *)
fun reasonable_date(d: DATE): bool =
    let
        val year = #1 d
        val month = #2 d
        val day = #3 d

        (* We assume the year is strictly positive *)
        fun is_leap_year(year: int): bool =
            (year mod 4 = 0) 
            andalso
            (year mod 100 <> 0 orelse year mod 400 = 0)

        (* Get a list of the days in each month *)
        fun get_days_in_months(is_leap_year: bool): int list =
            if is_leap_year 
                then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            else 
                [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        (* 
         * Get the list entry at the position specified.
         * First item is at position 1.
         * Assume pos >= 1 and pos <= size of nums list.
         *)
        fun get_at_position(nums: int list, pos: int): int =
            if pos = 1
                then hd nums
            else 
                get_at_position(tl nums, pos - 1)
    in
        if year <= 0 orelse month <= 0 orelse month > 12 orelse day <= 0
            then false
        else 
            let
                val day_list = get_days_in_months(is_leap_year(year))
                val days_in_month = get_at_position(day_list, month)
            in
                day <= days_in_month
            end
    end