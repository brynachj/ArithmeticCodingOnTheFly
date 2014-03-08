import Learner
import Arith_enc_states_scaling
import System.IO


main = do
     putStrLn "For instructions on how to encode, type 'encode'" 
     putStrLn "For instructions on how to decode, type 'decode'" 
     putStrLn "For instructions on how to learn models, type 'learn'" 
     putStrLn "For instructions on how to generate, type 'generate'" 
     putStrLn "For instructions on changing a model to a dyadic model, type 'dyadic'" 
     putStrLn "For instructions on how to translate a string using two models, type 'translate'"
     putStrLn "If you want to create a markov chain from characters with a priori knowledge of probabilities, type 'train'"
     putStrLn "To quit, type 'quit'"
     whatToDo <- getLine
     if whatToDo == "encode"
        then do putStrLn "----------"
                putStrLn "-ENCODING-"
                putStrLn "----------"
                putStrLn "The encoding part of this program takes a .txt file as an input and returns a .txt file in the same directory as an output."
                putStrLn "The output file contains the model used to encode along with the encoded bitstream."
                putStrLn "The encoder can either take a predefined model on which to encode the input, or it can train a model on the input before encoding (this is a longer process)"
                putStrLn "To encode a file with a pre-defined model, type 'l'. To train the input as a model, type anything else:"
                mOrL <- getLine
                if mOrL == "l"
                   then do putStrLn "Type the filepath of the .txt file to be encoded:"
                           filepath1 <- getLine
                           putStrLn "Type the filepath for the output to be written to:"
                           filepath2 <- getLine
                           putStrLn "Type the filepath of the .txt file containing the model:"
                           mod <- getLine
                           encodeFromFileWModel filepath1 mod filepath2
                           main
                   else do putStrLn "Type the filepath of the .txt file to be encoded:"
                           filepath1 <- getLine
			   putStrLn "Type the filepath for the model to be written to:"
			   filepath3 <- getLine
                           putStrLn "Type the filepath for the output to be written to:"
                           filepath2 <- getLine
                           encodeFromFile filepath1 filepath2 filepath3
                           main
        else do 
          if whatToDo == "decode"
              then do putStrLn "----------"
                      putStrLn "-DECODING-"
                      putStrLn "----------"
                      putStrLn "The decoding part of this program takes a .txt file as an input and returns a .txt file in the same directory as an output."
                      putStrLn "The output file contains the decoded character stream."
                      putStrLn "The decoder takes an encoded text file and a predefined model on which to decode the input."
                      putStrLn "Type the filepath of the .txt file to be decoded:"
                      filepath1 <- getLine
                      putStrLn "Type the filepath of the .txt file containing the model:"
                      mod <- getLine
                      putStrLn "Type the filepath for the output to be written to:"
                      filepath2 <- getLine
                      decodeFromFileWModel filepath1 mod filepath2
                      main
             else do
               if whatToDo == "generate"
                   then do putStrLn "----------"
                           putStrLn "-GENERATE-"
                           putStrLn "----------"
                           putStrLn "The generating part of this program takes a model filepath and an integer along with a filename for the output."
                           putStrLn "It trains a model on the given string and produces an output the length of the integer given."
                           putStrLn "Type the filepath of the model to be used to generate:"
                           mod <- getLine
                           putStrLn "Type the length of the string to output:"
                           strLen <- getLine
                           putStrLn "Type the name of the file to output the stream to:"
                           dest <- getLine
                           putStrLn "Type a number between 0 and 1000 to use as the random seed:"
                           ran <- getLine
                           genFromModelFile mod (read strLen::Int) (read ran::Int) dest
                           main
	        else do
                  if whatToDo == "learn"
		      then do putStrLn "----------"
                              putStrLn "-LEARNING-"
                              putStrLn "----------"
                              putStrLn "The learning part of this program takes a string or a .txt file and outputs a model based on the given input."
			      putStrLn "To learn from a file, type 'file'. To learn from a string, type anything else:"
                              mOrL <- getLine
 			      if mOrL == "file"
                                 then do putStrLn "Type the filepath of the .txt file to learn from:"
                                         filepath1 <- getLine
                                         putStrLn "Type the filepath for the model to be written to:"
                                         filepath2 <- getLine
					 putStrLn "There are two types of markov chain to learn." 
					 putStrLn "One is based on character frequencies."
					 putStrLn "The other is based on adjacent character pair frequencies."
					 putStrLn "To train using adjacent character pairs, type 'pairs'. Otherwise, type anything else:"
                                         decide <- getLine
					 if decide == "pairs"
					    then do trainStringFromFile filepath1 filepath2
                                            else do frequencyLearnFromFile filepath1 filepath2
                                         main
                                 else do putStrLn "Type the string for the model to learn from:"
                                         string <- getLine
                                         putStrLn "Type the filepath for the model to be written to:"
                                         filepath2 <- getLine
					 putStrLn "There are two types of markov chain to learn." 
					 putStrLn "One is based on character frequencies."
					 putStrLn "The other is based on adjacent character pair frequencies."
					 putStrLn "To train using adjacent character pairs, type 'pairs'. Otherwise, type anything else:"
                                         decide <- getLine
					 if decide == "pairs"
					    then do trainStringToFile string filepath2
                                            else do writeFile filepath2 ([head(string)] ++ "\n" ++ (show(frequencyLearn string)))
                                         main
                
   	           else do
                     if whatToDo == "dyadic"
		         then do putStrLn "-------------------------"
                                 putStrLn "-DYADIC MODEL CONVERSION-"
                                 putStrLn "-------------------------"
                                 putStrLn "By default, the dyadic model conversion approsimates the model to the nearest (1/2)^16."
   			         putStrLn "To specify a different power of (1/2), type 'diff'. Otherwise type anything else:"
                                 mOrL <- getLine
 			         if mOrL == "diff"
                                    then do putStrLn "Type the power of (1/2) to approximate the probabilities to:"
                                            num <- getLine
					    putStrLn "Type the filepath of the .txt file to get the model from:"
                                            filepath1 <- getLine
                                            putStrLn "Type the filepath for the model to be written to:"
                                            filepath2 <- getLine
                                            toDyadGranFromFile filepath1 filepath2 (read num::Int)
                                            main
                                    else do putStrLn "Type the filepath of the .txt file to get the model from:"
                                            filepath1 <- getLine
                                            putStrLn "Type the filepath for the model to be written to:"
                                            filepath2 <- getLine
                                            toDyadFromFile filepath1 filepath2
                                            main
   	              else do
                        if whatToDo == "translate"
		            then do putStrLn "-------------"
                                    putStrLn "-TRANSLATING-"
                                    putStrLn "-------------"
   	   		            putStrLn "The translating function takes two models and an input."
   	   		            putStrLn "It encodes the input on the first model and decodes the encoded string using the second model." 
               	                    putStrLn "In this way, it translates the input from the first probability distribution to the second."
                                    putStrLn "Type the filepath of the input stream:"
                                    source <- getLine
				    putStrLn "Type the filepath of the first model (used to encode the input):"
                                    mod1 <- getLine
				    putStrLn "Type the filepath of the second model (used to decode the encoded input):"
                                    mod2 <- getLine
                                    putStrLn "Type the filepath for the translated input to be written to:"
                                    dest <- getLine
                                    translateFromFiles source mod1 mod2 dest
                                    main
   	                 else do
                           if whatToDo == "train"
		               then do putStrLn "----------"
                                       putStrLn "-TRAINING-"
                                       putStrLn "----------"
   	   		               putStrLn "The training function takes a list of characters in single quotes, and associated probabilities."
                                       putStrLn "For example: ('a',1%2),('b',1%3),('c',1%6)"
                                       putStrLn "OR"
                                       putStrLn "('1',1%6),('2',1%12),('3',1%12),('4',1%12),('5',1%12),('6',1%6),('7',1%12),('8',1%12),('9',1%12),('0',1%12),"
                                       putStrLn "Type the filepath for the model to be written to:"
				       dest <- getLine
				       putStrLn "Type the set of characters and probabilities, in the above format:"
				       model <- getLine
				       writeFile dest (show(step4 (read ("[('\167',[" ++ model ++ "])]")::[(Char, [(Char,Rational)])])))
                                       main
		             else do
                               if whatToDo == "quit"
                                   then return()
                                  else do
                                        main
