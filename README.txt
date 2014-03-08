FEB 18 readme

TODO: 
Train needs an overhaul. How to do this in a User Friendly way I do not know!!
Updates: Write in that 'Pair Learn' takes a long time.
Encode could/should have options if we're learning a model from it: 
 -- Is it to be a frequency model or a 1-char model? - 1-char takes A LOT longer
 -- Is the model to be dyadic or not? - Test this. Dyadic longer up to a certain point.

Changed the name of data type 'State' to 'Transition' since it's a much less confusing descrition.


FIXED:
decodeSym wasn't working for single state Mkv Chains. Now does (added
a guard for model length 1).
