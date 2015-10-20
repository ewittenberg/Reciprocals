#<p>1. ${sent1}<br /><p>How long did this take?&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<textarea cols="3" name="hours1" rows="1"></textarea> hour(s)<textarea cols="3" name="mins1" rows="1"></textarea> minute(s)<textarea cols="3" name="secs" rows="1"></textarea> second(s)</p>
from string import Template
AMTfile_out = open('AMTFileRatings.txt','w')

#make sure there are enough #s later~
sent = (['${sent'+`i`+'}' for i in range(1,48)])
#count = (['${count'+`i`+'}' for i in range(1,47)])


amt2 = Template('<p>$i. $${sent$i}<br /><span class="answertext">totally natural </span> <input name="sent$i" type="radio" value="1" /> &nbsp;&nbsp;&nbsp;<input name="sent$i" type="radio" value="2" />&nbsp;&nbsp;&nbsp;  <input name="sent$i" type="radio" value="3" />&nbsp;&nbsp;&nbsp; <input name="sent$i" type="radio" value="4" />&nbsp;&nbsp;&nbsp; <input name="sent$i" type="radio" value="5" />&nbsp;&nbsp;&nbsp;  <input name="sent$i" type="radio" value="6" />&nbsp;&nbsp;&nbsp; <input name="sent$i" type="radio" value="7" /><span class="answertext"> completely unnatural </span></p>\n')

for j in range(1,48):
    AMTfile_out.write(amt2.substitute(i=j))

# Close opend file
AMTfile_out.close()





