Data Science Capstone -- Coursera by Johns Hopkins University -- Text Prediction Engine Using Natural Language Processing
========================================================
author: G.A. Pagani
date: 12th August 2017
width: 1900
height: 1100

<div align='center'>
<img src="https://d3njjcbhbojbot.cloudfront.net/api/utilities/v1/imageproxy/https://coursera-course-photos.s3.amazonaws.com/3f/08f9900d1311e48743bfe1d327ce7a/CourseTrackLogo.jpg" width=700 height=580>




Background: 
========================================================
<font size = "15px">
We write more and more using computers,
smartphones and tablet and we are always in a hurry, trying to
text everywhere from the bus stop to the bed or even the toilet!!


<div align='center'>
<img src="http://images.wisegeek.com/blonde-woman-using-smart-phone-on-bus.jpg" width=300 height=200>
<img src="http://is3.mzstatic.com/image/thumb/Purple117/v4/b4/84/b5/b484b5b9-633d-5fc3-a347-38a4614665bb/source/175x175bb.jpg" width=200 height=200>
<img src="https://abs.twimg.com/icons/apple-touch-icon-192x192.png" width=200 height=200>
<img src="http://phandroid.s3.amazonaws.com/wp-content/uploads/2014/01/blackberry-texting.jpg" width=300 height=200>
</div>

Why not making writing text simpler and even faster?

<div align='center'>
<img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSka3lTVKqlayUuHls56yK4O-cGm5Y95zsap2KAQHywIojB9KalRg" width=300 height=200>
</div>

  
The idea is to help you have your sentences written faster by 
predicting what word you are going to type next!

</font>

Approach:
======================================================
<font size = "15px">
- The software created is able to predict the word that follows a
certain written text, by computing the probability of possible following words.
- The underlying probabilistic model of the language is a probability chain 
of observing a sequence of words in that order. 
- To simplify the model we assume
that a word to appear in a sentence depends on the previous k-words only. 
- This simplification is known as Markov assumtion.

<div align='center'>
<img src="http://i2.cdn.cnn.com/cnnnext/dam/assets/130428160843-texting-generic-horizontal-large-gallery.jpg" width=500 height=300>
<img src="https://s-media-cache-ak0.pinimg.com/originals/bd/a5/8e/bda58e89eb992a835c0f235d8d702aee.jpg"
 width=300 height=300>
 <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR3rY9G_qSMgh6cuBA145-vU511i2KtMnIaE88ILc6gEr7UkmdFXQ" width=500 height=300>
</div>
</font>              


The App:
=====================================================
<font size = "8px">

- The software considers k up to 5: to predict the following word in a sentence at most the previous 5 words (5-gram in the Natural Language processing community) are used. 
                                  
- Kats Back-Off model is implemented in order to account for unseen words combination/chains.
- This method helps to generalize to word combinations that might be possible, but not directly present as n-grams in the training set.                                  
                                  
- The app in this prototype is very easy to use: just enter a phrase (or at least one word) in the 
                                  text area and then click on the button <i>Predict next word</i>.
- The app is available at <a href="https://andreapago.shinyapps.io/textPredictCapstone/">https://andreapago.shinyapps.io/textPredictCapstone/</a>
                                  
<div align='center'>
<img src="https://u1993096.dl.dropboxusercontent.com/u/1993096/app.png" width=750 height=275>
</div>

</font> 
                                  
App features:
=====================================================
<font size = "8px">

- The text analyzed to compute the word chains and thus the probabilities of 
                                  new words comes from an English text corpus composed of 3 types of documents:
                                  Tweets, Blog Posts, and News articles. 
- The corpus is combined of about 4 million lines of text
                                  totalling to more than 100 millions words. 
- For this working example a small random sample from the corpus is used that is 0.5% of the total lines of text available. 
- The choice of such a sample is 
                                  motivated by the limitations in the computational resources of the shiny platform and the fast 
                                  response required by the app.
- The result with this small sample allows on one hand a relative small set of word combination observed, on the other hand
                                  the app is fast and producing result (also in a system with very limited resources like the shiny app).
- When testing on a randomly selected test set from the corpus the predicted word is correct in 5.5% of the cases. This is due to the heavy sampling done from the original text due to the performace constraints.
</font> 
