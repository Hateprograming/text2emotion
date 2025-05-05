# text2emotion
A Simple R Package for Emotion Detection from Text

This package allows users to analyze text and classify emotions such as happiness, sadness, anger, fear, and neutrality. It combines text preprocessing, TF-IDF feature extraction, and Random Forest classification to predict emotions and map them to corresponding emojis for enhanced sentiment visualization.

Main features:
- Text preprocessing: clean, normalize, and handle slang and negations.
- TF-IDF vectorization of text data.
- Emotion classification using a trained model.
- Convert predicted emotion into a corresponding emoji.

Example usage in R:

library(text2emotion)

predict_emotion_with_emoji("I'm feeling great today!")

Output: I'm feeling great today!😊

This package is useful for sentiment analysis, chatbot response enhancement, social media text analysis, and more.

