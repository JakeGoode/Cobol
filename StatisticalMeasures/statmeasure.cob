*> Name: Jake Goode
*> Student Number: 1202742
*> Date: 02-19-2024
*> Assignment 3: Statistical Measures

identification division.
program-id. statmeasure.

environment division.
input-output section.
file-control.
select standard-input assign to keyboard.
select standard-output assign to display.
select input-file assign to dynamic fname-inp
    organization is line sequential
    file status is file-stat.

data division.
file section.
fd standard-input.
    01 std-input      pic x(80).
fd standard-output.
    01 std-output     pic x(80).
fd input-file.
    01 sample-input   pic x(80).

working-storage section.
77 sum-of-x-mul   pic 9(10)v9(28).
77 sum-of-sr      pic 9(10)v9(28).
77 sum-of-sx2     pic 9(10)v9(28).
77 sum-of-x-sqr   pic 9(10)v9(28).
77 sum-of-x       pic s9(10)v9(28).
77 n              pic s9(4) value 0.
77 mean           pic s9(6)v9(2).
77 i              pic s9(4).
77 feof           pic a(1) value 'N'.
77 fname-inp      pic x(30).
77 file-stat      pic xx.

01 array-area.
    02 x           pic s9(6)v9(2) occurs 1000 times.

01 input-value-record.
    02 in-x        pic s9(6)v9(2).

01 output-underline.
    02 filler      pic x(32) value
                  "--------------------------------".

01 output-data-line.
    02 filler      pic x(22) value spaces.
    02 out-x       pic -(6)9.9(2).

01 output-mean.
    02 filler      pic x(22) value "  Mean =              ".
    02 out-mean    pic -(6)9.9(2).

01 output-std-dev.
    02 filler      pic x(22) value "  Standard Deviation =".
    02 std-deviation    pic -(6)9.9(2).

01 output-geo-mean.
    02 filler      pic x(22) value "  Geometric Mean =    ".
    02 geometric-mean   pic -(6)9.9(2).

01 output-harm-mean.
    02 filler      pic x(22) value "  Harmonic Mean =     ".
    02 harmonic-mean    pic -(6)9.9(2).

01 output-root-mean.
    02 filler      pic x(22) value "  Root Mean Square =  ".
    02 root-mean-sq     pic -(6)9.9(2).

procedure division.
    *>Open keyboard and screen use.
    open input standard-input, output standard-output.

    display " ".
    display "Enter a filename to display the statistical measures: ".
    
    *>Get the file from user.
    accept fname-inp.
    open input input-file.

    *>Check if file is available, otherwise stop the program.
    if (file-stat = "35") then
        display "File does not exist or cannot be opened."
        display " "
        stop run
    end-if.

    *>Gather the data from the file.
    perform input-loop until feof = 'Y' or n > 1000.

    *>Calculate each statistic.
    perform calc-body.

    *>Output numbers in file and calculated statistics to screen.
    perform output-body.

    *>Close the file and stop the program.
    perform end-of-job.

calc-body.
    *>Calculate the mean (total/number of items).
    perform mean-comp.
    
    *>Calculate the standard deviation [sqrt(((x(1)-mean)^2+..+(x(n)-mean)^2)/n)].
    perform stan-dev.
    
    *>Calculate the geometric mean [(x(1)⋅x(2)⋅x(3)⋅…⋅x(n))^(1/n)].
    perform geo-mean.

    *>Calculate the harmonic mean [n/SR].
    perform harm-mean.

    *>Calculate the root mean square [sqrt(Sx2/n)].
    perform root-mean.

output-body.
    display " ".
    display "      Statistical Measures".
    *>--------------------------------
    display output-underline.
    display "                     Data Values".
    display output-underline.
    
    *>Prints numbers from file to screen.
    perform output-loop.
    display output-underline.
    
    *>Prints mean.
    display output-mean.
    
    *>Prints standard deviation.
    display output-std-dev.
    
    *>Prints geometric mean.
    display output-geo-mean.
    
    *>Prints harmonic mean.
    display output-harm-mean.
    
    *>Prints root mean square.
    display output-root-mean.
    display " ".

*>Loop to get all numbers from file to calculate statistics.
input-loop.
    *>Read into array until end of file.
    read input-file into input-value-record
        at end move 'Y' to feof
        not at end
            *>Increase n by 1 and add to array.
            compute n = n + 1
            move in-x to x(n)
    end-read.

*>Loop to output numbers in array to screen.
output-loop.
    perform varying i from 1 by 1 until i > n
        move x(i) to out-x
        display output-data-line
    end-perform.

*>Calculates the statistical mean.
mean-comp.
    move 0 to sum-of-x.

    perform varying i from 1 by 1 until i > n
        compute sum-of-x = sum-of-x + x(i)
    end-perform.

    compute mean rounded = sum-of-x / n.
    move mean to out-mean.

*>Calculates the statistical standard deviation.
stan-dev.
    move 0 to sum-of-x-sqr.

    perform varying i from 1 by 1 until i > n
        compute sum-of-x-sqr = sum-of-x-sqr + (x(i) - mean) ** 2
    end-perform.

    compute std-deviation rounded = (sum-of-x-sqr / n) ** 0.5.

*>Calculates the statistical geometric mean.
geo-mean.
    *>Move 1 to sum so multiplication will work properly.
    move 1 to sum-of-x-mul.

    perform varying i from 1 by 1 until i > n
        *>sum-of-x-mul * [x(i)^(1/n)]
        compute sum-of-x-mul = sum-of-x-mul * (x(i) ** (1 / n))
    end-perform.

    compute geometric-mean rounded = sum-of-x-mul.

*>Calculates the statistical harmonic mean.
harm-mean.
    move 0 to sum-of-sr.

    perform varying i from 1 by 1 until i > n
        *>SR = 1/x(1) + 1/x(2) + … + 1/x(n).
        compute sum-of-sr = sum-of-sr + (1 / x(i))
    end-perform.

    compute harmonic-mean rounded = n / sum-of-sr.

*>Calculates the statistical root mean square.
root-mean.
    move 0 to sum-of-sx2.

    perform varying i from 1 by 1 until i > n
        *>Sx2 = x(1)⋅x(1) + x(2)⋅x(2) + … + x(n)⋅x(n).
        compute sum-of-sx2 = sum-of-sx2 + (x(i) * x(i))
    end-perform.

    compute root-mean-sq rounded = function sqrt (sum-of-sx2 / n).

*>Closes the input file and stops the program when complete.
end-of-job.
    close input-file.
    stop run.
