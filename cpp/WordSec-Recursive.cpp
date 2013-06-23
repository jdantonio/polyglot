//*********************************************************************
//  CODE FILENAME:  DANTONIO-Assn1-DLLProg.cpp
//
//  DESCRIPTION:    A program to analyze words in a text file. The text
//                  files contain paragraphs of sentences of English
//                  words and possibly numbers. The program:
//                  * Reads, counts and organizes the words into lists
//                    based on starting letters.
//                  * Words containing any digits are ignored.
//                  * Determines the number of unique words in the text
//                    file that begin with each alphabetic letter.
//                  * Determines which letter or letters begin(s) the
//                    most unique words.
//                  * Outputs resulst to standard out.
//
//  DATE:           1/22/1012
//  DESIGNER:       Jerry D'Antonio
//
//  FUNCTIONS:      int main(int, char*[])
//                  void InitWorkLists(WorkLists&);
//                  void FreeWorkLists(WorkLists&);
//                  string GetFilenameFromCommandLine(int, char*[]);
//                  void PauseProgram(void);
//                  void PrintUsage(const char*);
//                  bool OpenInputFile(string, fstream&);
//                  void PrintFileError(void);
//                  int ReadWordsFromFile(WorkLists&, fstream&);
//                  string ValidateWord(string);
//                  bool IsWordInList(WordList, string);
//                  WordList InsertWordIntoList(WordList&, string);
//                  void PrintHeader(string, int);
//                  void PrintWorkLists(WorkLists&);
//                  int PrintWordList(WordList&);
//**********************************************************************

#include <iostream>
#include <fstream>
#include <cstddef>

using namespace std;

//**********************************************************************
// Constants

const int WORK_LIST_ARRAY_SIZE = 26;
const int WORK_LIST_ARRAY_OFFSET = 'A';

#define WORK_LIST_INDEX(c) (c - WORK_LIST_ARRAY_OFFSET)

//**********************************************************************
// Type definitions

// Node in a double linked list of words
typedef struct WordNode {
  string word;          // the word
  WordNode* next;       // pointer to next node
  WordNode* previous;   // pointer to previous node
} WordNode;

// Double linked list of words
typedef WordNode* WordList;

// Array of word lists, one per letter in the English alphabet
typedef WordList WorkLists[WORK_LIST_ARRAY_SIZE];

//**********************************************************************
// Function prototypes

void InitWorkLists(WorkLists&);
void FreeWorkLists(WorkLists&);
  void FreeNode(WordNode*);
string GetFilenameFromCommandLine(int, char*[]);
void PauseProgram(void);
void PrintUsage(const char*);
bool OpenInputFile(string, fstream&);
void PrintFileError(void);
int ReadWordsFromFile(WorkLists&, fstream&);
    string ValidateWord(string);
    bool IsWordInList(WordList, string);
    WordList InsertWordIntoList(WordList&, string);
void PrintHeader(string, int);
void PrintWorkLists(WorkLists&);
    int PrintWordList(WordList&);


//*********************************************************************
//  FUNCTION:     main
//  DESCRIPTION:  Oversees entire program. Assigns most of the work to
//                other functions.
//  INPUT:
//      Command Line: The first command line argument must be the path
//                    to the text input file.
//  OUTPUT:  
//      Display: Aggregate data for all file/word operations.
//      Return Val: Zero (0) on success, non-zero on error.
//**********************************************************************
int main(int argc, char *argv[]) {

  // Variable declarations
  WorkLists workLists;    // Lists of unique words in file
  string filename;        // The name of the input file
  fstream inputFile;      // File stream for readin input file
  int totalWordCount;     // Total words read from file

  filename = GetFilenameFromCommandLine(argc, argv);

  // check file name and open file
  // NOTE: Exits program on file error
  if (filename.empty()) {
    
    // file name not given
    PrintUsage(argv[0]);
    PauseProgram();
    return(1);

  } else if (false == OpenInputFile(filename, inputFile)) {

    // error opening file
    PrintFileError();
    PauseProgram();
    return(2);
  } // end else
  
  // process file, build word lists
  InitWorkLists(workLists);
  totalWordCount = ReadWordsFromFile(workLists, inputFile);
  inputFile.close();

  // output results to the console
  PrintHeader(filename, totalWordCount);
  PrintWorkLists(workLists);

  // free allocated resource
  FreeWorkLists(workLists);

  // pause program output
  PauseProgram();

  return(0);
} // end main


//*********************************************************************
//  FUNCTION:     InitWorkLists
//  DESCRIPTION:  Initializes a WorksList array by setting all elements
//                to NULL.
//  INPUT:
//      Parameters:
//        WorkLists& list - The list (array) to be initialized.
//  OUTPUT:  
//      Return Val: Description of data returned by a function
//      Parameters:
//        WorkLists& list - The input array with all elements initialized.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void InitWorkLists(WorkLists& list) {

  // initialize all array elements to NULL
  for (int i = 0; i < WORK_LIST_ARRAY_SIZE; i++) {
    list[i] = NULL;
  } // end for

} // end InitWorkLists


//*********************************************************************
//  FUNCTION:     FreeWorkLists
//  DESCRIPTION:  Free all memory dynamically allocated on a WorkList.
//                Does NOT update the pointers in the list. To be
//                reused the WorkList must be initialized again.
//  INPUT:
//      Parameters:
//        WorkLists& list - The list to be operated on.
//  OUTPUT:  
//      Parameters:
//        WorkLists& list - The list with all dynamically allocated
//                          memory freed.
//  PERFORMANCE:  f(n) = n
//**********************************************************************
void FreeWorkLists(WorkLists& list) {

  WordNode* currentNode = NULL;   // Pointer to current node in list

  // iterate over all elements in the array
  for (int i = 0; i < WORK_LIST_ARRAY_SIZE; i++) {

    // get the head node for the next list
    currentNode = list[i];

    // recursively free all the nodes in the list
    if (currentNode != NULL) {
      FreeNode(list[i]);
    }

  } // end for

} // FreeWorkLists


//*********************************************************************
//  FUNCTION:     FreeNode
//  DESCRIPTION:  Recursively frees all nodes in a WorkLists element.
//  INPUT:
//      Parameters:
//        WordNode current - The node to be operated on.
//**********************************************************************
void FreeNode(WordNode* current) {
  if (current->next != NULL) {
    FreeNode(current->next);
  }
  delete current;
}


//*********************************************************************
//  FUNCTION:     GetFilenameFromCommandLine
//  DESCRIPTION:  Description of purpose of function
//  INPUT:
//      Parameters:
//        int argc - Count of command line parameters
//        char *argv[] - Collection of command line parameters.
//  OUTPUT:  
//      Return Val: std::string containing the path of the input file
//                  or an empty string if no file path given.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
string GetFilenameFromCommandLine(int argc, char *argv[]) {

  string filename;    // File name of input file

  if (argc > 1) filename = argv[1];

  return(filename);
} // GetFilenameFromCommandLine


//*********************************************************************
//  FUNCTION:     PauseProgram
//  DESCRIPTION:  Pause program execution and wait for user input.
//
//  NOTE: This pause exists mainly to support running the program in
//        an IDE. It causes difficulties when automating testing with
//        tools like RSpec. The body of this function should be
//        commented out when performing automated testing.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void PauseProgram(void) {

  cout << endl << "Press ENTER key to resume program...\n";
  cin.get();
}


//*********************************************************************
//  FUNCTION:     PrintUsage
//  DESCRIPTION:  Display progam usage information to standard out.
//  INPUT:
//      Parameters:
//        const char* program - The name of the running program.
//  OUTPUT:  
//      Display: Program usage information.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void PrintUsage(const char* program) {

  cout << "Usage: " << program << " FILENAME"
       << endl
       << "Analyzes words in a text file containing paragraphs of sentences of English words and possibly numbers."
       << endl
       << "\tFILENAME\t Relative or absolute path to the input file."
       << endl;;

} // PrintUsage


//*********************************************************************
//  FUNCTION:     OpenInputFile
//  DESCRIPTION:  Open the input text file.
//  INPUT:
//      Parameters:
//        std::string filename - Path to the input file.
//        std::fstream file - The file object to use when opening the
//                            input file.
//  OUTPUT:  
//      Return Val: Boolean indicating success of the file open
//                  operation.
//      Parameters:
//        std::fstream file - Initialized with the input file open on
//                            success.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
bool OpenInputFile(string filename, fstream& file) {

  file.open(filename.c_str(), fstream::in);

  return(file.good());
} // OpenInputFile


//*********************************************************************
//  FUNCTION:     PrintFileError
//  DESCRIPTION:  Print an error message to standard out when there
//                is a problem opening the input text file.
//  INPUT:
//      Parameters: None
//  OUTPUT:  
//      Display: Error message.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void PrintFileError(void) {

  cout << "FILE ERROR! There was an error opening/reading the input file." << endl;

} // PrintFileError


//*********************************************************************
//  FUNCTION:     ReadWordsFromFile
//  DESCRIPTION:  Read all words from the input text file, validate
//                each word, the store each unique, valid word in a
//                WorkList.
//  INPUT:
//      Parameters:
//        WorkLists& list - Repository for all unique words.
//        std::fstream file - The input file object.
//  OUTPUT:  
//      Return Val: Total number of valid words read from the file.
//      Parameters:
//        WorkLists&list - Updated with all unique words read in.
//  CALLS TO:
//      ValidateWord
//      IsWordInList
//      InsertWordIntoList
//  PERFORMANCE:  f(n) = n
//**********************************************************************
int ReadWordsFromFile(WorkLists& list, fstream& file) {

  int wordCount = 0;    // Count of words read from file

  string word;          // Current word being read
  int index;            // Index in the work list for the current letter

  // read from the file until end of file reached
  while ( ! file.eof() ) {

    // get the next word from the file
    file >> word;

    // ignore blank lines
    if ( ! word.empty()) {

      wordCount++;
      word = ValidateWord(word);

      // skip invalid words
      if (! word.empty()) {

        index = WORK_LIST_INDEX(word[0]);

        // add the word to the list only if not already encountered
        if ( ! IsWordInList(list[index], word)) {
          list[index] = InsertWordIntoList(list[index], word);
        } // end if

        // empty the input buffer to prevent erroneous next read
        word.clear();

      } // if
    } // if
  } // end

  return(wordCount);
} // ReadWordsFromFile


//*********************************************************************
//  FUNCTION:     ValidateWord
//  DESCRIPTION:  Check single word against validation rules, strip it
//                of unnecessary characters, and make it upper case.
//  INPUT:
//      Parameters:
//        std::string word - The word to be validated.
//  OUTPUT:  
//      Return Val: Upper case version of the input word with all
//                  unnecessary characters stripped. An empty word if
//                  the input is invalid.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
string ValidateWord(string word) {

  // Alphabetic characters, used for stripping punctuation
  const char alphabetics[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  // Numeric characters, used for excluding invalid words
  const char numerals[] = "0123456789";

  size_t index;   // Index in word of character being checked

  // check word for numerals
  index = word.find_first_of(numerals);

  if (index != string::npos) {
  
    // dump the word, it is invalid
    word.clear();

  } else {

    // lowercase the word
    transform(word.begin(), word.end(), word.begin(), ::toupper);

    // strip punctuation from front of word
    index = word.find_first_of(alphabetics);
    if (index > 0) word.erase(0, index);

    // strip punctuation from end of word
    index = word.find_last_of(alphabetics);
    if (index != string::npos) word.erase(index + 1, word.size() - index - 1);

  } // if

  return(word);
} // ValidateWord


//*********************************************************************
//  FUNCTION:     IsWordInList
//  DESCRIPTION:  Determine if a given word is already in a WordList.`
//  INPUT:
//      Parameters:
//        WordList list - List to be searched.
//        std::string word - Word to search for.
//  OUTPUT:  
//      Return Val: Boolean indicating if the word is in the list.
//  PERFORMANCE:  f(n) = n/26
//**********************************************************************
bool IsWordInList(WordList list, string word) {
  
  bool found = false;             // Indicates the word was found in the list
  WordNode* currentNode = list;   // The current node being interrogated

  // skip the search when there are no words
  if (list != NULL) {

    // traverse the entire list, exit loop if word is found
    while (! found && currentNode != NULL) {

      if (currentNode->word == word) {
        found = true;
      } else {
        currentNode = currentNode->next;
      } // if
    } // while
  }

  return(found);
} // IsWordInList


//*********************************************************************
//  FUNCTION:     InsertWordIntoList
//  DESCRIPTION:  Insert a word at the head of a WordList.
//  INPUT:
//      Parameters:
//        WordList& list - List in which to insert the word.
//        std::string word - Word to insert within the list.
//  OUTPUT:  
//      Return Val: Pointer to the new head node of the list.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
WordList InsertWordIntoList(WordList& list, string word) {

  WordNode* headNode = (WordNode*) list;   // The node at the head of the list
  
  if (headNode == NULL) {

    // create a new head node
    headNode = new WordNode;
    headNode-> word = word;
    headNode->next = NULL;
    headNode->previous = NULL;

  } else {

    // insert to head of list
    headNode->previous = new WordNode;
    headNode->previous->word = word;
    headNode->previous->next = headNode;
    headNode->previous->previous = NULL;
    headNode = headNode->previous;
  } // end if

  return((WordList) headNode);
} // InsertWordIntoList


//*********************************************************************
//  FUNCTION:     PrintHeader
//  DESCRIPTION:  Display the output 'header' information to standard
//                out. This includes the input file name and total
//                words processed.
//  INPUT:
//      Parameters:
//        std::string filename - Name of the input file.
//        int totalWords - Total number of words read from the file.
//  OUTPUT:  
//      Display: Text, file name, and word count to standard out.
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void PrintHeader(string filename, int totalWords) {

  cout << "Results for file " << filename << ":  "
       << totalWords << " total words processed."
       << endl << endl;

} // PrintHeader


//*********************************************************************
//  FUNCTION:     PrintWorkLists
//  DESCRIPTION:  Prints information regarding all word lists to
//                standard output.
//  INPUT:
//      Parameters:
//        WorkLists& list - Lists of words to process and print.
//  OUTPUT:  
//      Display: Information about all words in all lists.
//  CALLS TO:
//      PrintWordList
//  PERFORMANCE:  f(n) = k
//**********************************************************************
void PrintWorkLists(WorkLists& list) {

  int wordCount = 0;          // Number of words read from a list
  int totalWords = 0;         // Total words read from the file
  int highestCount = 0;       // Most words from a single letter
  char letter;                // The current letter being printed
  string mostCommonLetters;   // Set of letters with highest frequency

  // iterate over the entire array
  for (int i = 0; i < WORK_LIST_ARRAY_SIZE; i++) {

    // skip over letters with no words
    if (list[i] != NULL) {

      letter = list[i]->word[0];

      wordCount = PrintWordList(list[i]);

      totalWords += wordCount;

      // track which letters have the highest count
      if (wordCount == highestCount) {

        // current letter is tied for highest count
        mostCommonLetters.append(1, letter);

      } else if (wordCount > highestCount) {

        // current letter is new highest count
        mostCommonLetters = letter;
        highestCount = wordCount;

      } // end if
    } // end if
  } // end for

  // output aggregate list totals
  cout << endl
       << "There were " << totalWords << " unique words in the file."
       << endl
       << "The highest word count was " << highestCount << "."
       << endl << endl
       << "Letter(s) that began words " << highestCount << " times were"
       << endl;

  // output information for letters with the highest word count
  for (size_t i = 0; i < mostCommonLetters.size(); i++) {

    cout << "\t'" << (char) tolower(mostCommonLetters[i])
         << "'/'" <<  mostCommonLetters[i]<< "'"
         << endl;
  } // end for

} // PrintWorkLists


//*********************************************************************
//  FUNCTION:     PrintWordList
//  DESCRIPTION:  Print information about each per-letter word list
//                to standard out.
//  INPUT:
//      Parameters:
//        WordList& list - List of words to be processed and printed.
//  OUTPUT:  
//      Display: Information about all words in the list.
//  PERFORMANCE:  f(n) = n/26
//**********************************************************************
int PrintWordList(WordList& list) {

  int wordCount;        // Number of words in the list
  string descriptor;    // "word" or "words" depending on wordCount

  WordNode* currentNode = (WordNode*) list;   // current node, set to head
  char letter = currentNode->word[0];         // letter of the current list

  wordCount = 1;    // count the first node in the list

  // traverse the entire word list
  while (currentNode->next != NULL) {
    // increment the count and move forward in the list
    wordCount++;
    currentNode = currentNode->next;
  }

  // output the count information for the current list
  descriptor = ( wordCount == 1 ? "word" : "words" );
  cout << wordCount << " " << descriptor << " beginning with '"
       << (char) tolower(letter) << "'/'" << letter << "':"
       << endl;

  // output the list of words with proper formatting
  cout << "\t";
  while (currentNode != NULL) {

    cout << currentNode->word;

    // check for last word and output appropriate formatting
    if (currentNode->previous != NULL) {
      cout << ", ";
    } else {
      cout << endl;
    }

    // advance in the list
    currentNode = currentNode->previous;
  }

  return(wordCount);
} // PrintWordList
