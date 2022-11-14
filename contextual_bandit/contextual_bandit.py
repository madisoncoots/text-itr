import numpy as np

class ContextualBandit:

	def __init__(self):
		self.actions = ("text", "do not text")

	def predict(self, context, history):
		# Given the current context vector and the past history in the form of 
        # [(context), (action), reward]
        # return an action
        score = np.square(np.dot(context.T, self.weights) + self.bias)
        return bucketize_action(score)

    def predict_no_update(self, context, history):
    	return self.predict(context, history)

    def update(self, context, action, reward):
    	pass