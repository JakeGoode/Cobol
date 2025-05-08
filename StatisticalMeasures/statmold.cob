*> Name: Jake Goode
*> Student Number: 1202742
*> Date: 02-19-2024
*> Assignment 3: Statistical Measures

identification division.
program-id. statmold.

environment division.
input-output section.
file-control.
select input-file assign to "nums.txt"
   organization is line sequential
   file status is file-stat.
select output-file assign to "statout.txt"
   organization is line sequential.
select standard-output assign to display.

data division.
file section.
fd input-file.
   01 sample-input   pic x(80).
fd output-file.
   01 output-line   pic x(80).
fd standard-output.
   01 std-output   pic x(80).

working-storage section.
77 sum-of-x-sqr   pic 9(14)v9(2).
77 sum-of-x       pic s9(10)v9(2).
77 n              pic s9(4).
77 mean           pic s9(6)v9(2).
77 i              pic s9(4).
77 file-stat      pic xx.

01 array-area.
   02 x           pic s9(6)v9(2) occurs 1000 times.

01 input-value-record.
   02 in-x        pic s9(6)v9(2).
   02 filler      pic x(72).

01 output-title-line.
   02 filler      pic x(28) value
                  " MEAN AND STANDARD DEVIATION".

01 output-underline.
   02 filler      pic x(28) value
                  "----------------------------".

01 output-col-heads.
   02 filler      pic x(10) value spaces.
   02 filler      pic x(11) value "DATA VALUES".

01 output-data-line.
   02 filler      pic x(10) value spaces.
   02 out-x       pic -(6)9.9(2).

01 output-results-line-1.
   02 filler      pic x(9) value " MEAN=   ".
   02 out-mean    pic -(6)9.9(2).

01 output-results-line-2.
   02 filler      pic x(9) value " STD DEV=".
   02 std-deviation    pic -(6)9.9(2).

procedure division.
   *>Opens hardcoded input and output files for use.
   open input input-file, output output-file.

   *>Check if file is available, otherwise stop the program.
   if (file-stat = "35") then
      display "file does not exist."
      display " "
      close output-file
      stop run
   end-if.
   
   move zero to in-x.
   
   *>Loop until file terminator 999999.99 is encountered in file.
   perform proc-body
      until in-x is not less than 999999.98.
   
   *>Close the files and stop the program.
   perform end-of-job.

*>Gathers the numbers from input file, calculates the mean and
*>standard deviation and prints to output file.
proc-body.
   *>MEAN AND STANDARD DEVIATION.
   write output-line from output-title-line
      after advancing 0 lines.
   
   *>----------------------------
   write output-line from output-underline
      after advancing 1 line.
   
   *>DATA VALUES.
   write output-line from output-col-heads
      after advancing 1 line.
   write output-line from output-underline
      after advancing 1 line.
   
   move zero to sum-of-x.
   *>Reads first line from file into array.
   read input-file into input-value-record
      at end perform end-of-job.
   
   *>Loop to read remaining lines into array until terminator
   *>999999.99 is encountered.
   perform input-loop
      varying n from 1 by 1
      until n is greater than 1000 or in-x is not less than 999999.98.
   
   subtract 1 from n.
   divide n into sum-of-x giving mean rounded.

   move zero to sum-of-x-sqr.
   *>Calculates the sum of (x - mean)^2 for standard deviation.
   perform sum-loop
      varying i from 1 by 1
      until i is greater than n.
   compute std-deviation rounded = (sum-of-x-sqr / n) ** 0.5.
   
   write output-line from output-underline
      after advancing 1 line.
   move mean to out-mean.
   
   *>Prints mean to output file.
   write output-line from output-results-line-1
      after advancing 1 line.
   
   *>Prints standard deviation to file.
   write output-line from output-results-line-2
      after advancing 1 line.

*>Gathers the numbers from the input file, places into array, finds
*>the sum of all numbers, and prints each number to the output file.
input-loop.
   move in-x to x(n), out-x.
   
   write output-line from output-data-line
      after advancing 1 line.
   
   add x(n) to sum-of-x.
   
   *>Gets next line from input file.
   read input-file into input-value-record
      at end perform end-of-job.

*>Calculates the sum of (x - mean)^2 for standard deviation.
sum-loop.
   compute sum-of-x-sqr = sum-of-x-sqr + (x(i) - mean) ** 2.

*>Closes the input and output files and stops the program when complete.
end-of-job.
   close input-file, output-file.
   stop run.
