# 6.00 Problem Set 3
# 
# Hangman
#


# -----------------------------------
# Helper code
# (you don't need to understand this helper code)
import random
import string

WORDLIST_FILENAME = "words.txt"

def load_words():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = string.split(line)
    print "  ", len(wordlist), "words loaded."
    return wordlist

def choose_word(wordlist):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code
# -----------------------------------

# actually load the dictionary of words and point to it with 
# the wordlist variable so that it can be accessed from anywhere
# in the program
wordlist = load_words()

# your code begins here!
want_to_play = True
while (want_to_play):
    letter_bank = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k",\
                   "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v",\
                   "w", "x", "y", "z"]
    letter_reference = {"a":0, "b":1, "c":2, "d":3, "e":4, "f":5, "g":6, "h":7,\
                        "i":8, "j":9, "k":10, "l":11, "m":12, "n":13, "o":14,\
                        "p":15, "q":16, "r":17, "s":18, "t":19, "u":20, "v":21,\
                        "w":22, "x":23, "y":24, "z":25}
    guessed_word = ""
    number_of_guesses = 7
    first = True
    word = choose_word(wordlist)
    while(number_of_guesses > 0):
        print "My word is", len(word), "letters long."
        if first:
            for i in range(0, len(word)):
                guessed_word = guessed_word + "_"
        first = False
        for letter in range(0, len(guessed_word)):
            if letter == len(guessed_word) - 1:
                print guessed_word[letter]
            else:
                print guessed_word[letter], " ",
        print "You have", number_of_guesses, "guesses remaining."
        guess = str(raw_input("Which letter would you like to guess?"))
        accuracy = False
        index_list = []
        for j in range(0, len(letter_bank)):
            if guess == letter_bank[j]:
                for k in range(0, len(word)):
                    if word[k] == guess:
                        accuracy = True
                        index_list = index_list + [k,]
                #print "Index list: ",index_list #debug
                if accuracy:
                    print "Your guess was correct!"
                    list_convert = list(guessed_word)
                    #print "List_convert: ", list_convert #debug
                    #print "Guess is:", guess #debug
                    guessed_word = ""
                    for letter in range(0, len(list_convert)):
                        if letter in index_list:
                            list_convert[letter] = guess
                        guessed_word += str(list_convert[letter])
                       #print "Guessed word in loop is:", guessed_word #debug
                else:
                    print "Your guess was incorrect."
                    number_of_guesses -= 1
                letter_bank[j] = "Taken"
                break
            elif letter_bank[letter_reference[guess]] == "Taken":
                print "The letter", guess, "has already been used."
                view = str(raw_input("Would you like to view your available letters? (Y/N)"))
                if view == "Y" or view == "y":
                    for letter in range(0, len(letter_bank)):
                        if letter == len(letter_bank) - 1:
                            print letter_bank[letter]
                        else:
                            print letter_bank[letter],
                break
            else:
                if j == len(letter_bank) - 1:
                    print "Your guess was not a letter."
        winner = False
        for blank in range(0, len(guessed_word)):
            if guessed_word[blank] == "_":
                winner = False
                break
            winner = True
        if winner:
            print "You won the game!"
            print "The word was: ", word
            break
        if number_of_guesses == 0:
            print "You have run out of guesses."
            print "The word was: ", word
            
    decision = raw_input("Do you want to play again? (Y/N)")
    if decision == "Y" or decision == "y":
        want_to_play = True
    else:
        want_to_play = False
