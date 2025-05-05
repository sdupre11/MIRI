# note that base round() will round .5 "to the even digit" according to some IEC standard (see ?round)
# e.g round(x = .305, digits = 2) = .3 instead of .31
# so this homemade function from stack overflow fixes that
# https://stackoverflow.com/questions/12688717/round-up-from-5
# and then combines it with a format call to ensure the digits are shown
# https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r
round_to_digits <- function(x, digits, big_mark = ",") {
  sign <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z <- z * sign
  return(str_trim(string = format(x = z, nsmall = digits, big.mark = big_mark), side = "both"))
}


######################


# # test
# round(x = .305, digits = 2) # .3
# round(x = .315, digits = 2) # .32
# 
# # round_to_digits always will round up if digit is 5 or greater, unlike base round(), which rounds up "to the even digit"
# round_to_digits(x = .305, digits = 2) # .31
# round_to_digits(x = .315, digits = 2) # .32
# 
# # round_to_digits will show trailing zeros
# round(x = 33.0, digits = 1) # 33
# round_to_digits(x = 33.0, digits = 1) # 33.0
# 
# # round_to_digits will not show leading white space when using format()
# # format() tries to leave leading white space so that decimal places appear aligned when viewed in r console
# # but if data is output to csv/excel/html table, then this is unncessary and can cause unwanted complications
# round_to_digits(x = c(2.56, 1.05, 33, 120349.49450), digits = 1)
# round_to_digits(x = c(2.56, 1.05, 33, 120349.49450), digits = 1, big_mark = "")
