

#### sentiment analysis and function


#get the text files

pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

mytest = c("great you re here", "awesome experience", "You had a bad night", "she loves candy")

testsentiment = score.sentiment(mytest, pos, neg)

class(testsentiment)


score.sentiment = function(sentences, pos.words, neg.words, .progress = 'none')
{
  #parameters
  #sentences: vector of text to score
  #pos.words: vector of words of positve sentiment
  #neg words: pvector of words of negative sentiment
  #.progress: passed to laply() to control of of progress bar
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   #remove the punctuation
                   sentence = gsub("[[:punct:]]","",sentence)
                   #remove control characters
                   sentence = gsub("[[:cntrl:]]","",sentence)
                   #remove digits
                   sentence = gsub('\\d+',"",sentence)
                   
                   #define error handling function when trying to lower
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y= tolower(x)
                     return(y)
                   }
                   
                   
                   sentence = sapply(sentence, tryTolower)
                   
                   #split the sentences into words
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   #make the comparison
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress) 
  
  scores.df = data.frame(text=sentences, score = scores)
  return(scores.df)
}  
