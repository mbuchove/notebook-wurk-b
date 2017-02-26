/*
 * spam.cpp
 *
 *  Created on: Dec 1, 2016
 *      Author: mbuchove
 */

void test()
{
	// This writes "Hello"
	cerr << getFirstWord("!!Hello, Fred") << endl;

	  // This writes "greetings 9"
	string msg = "greetings, mom, how are you?";
	string result = getFirstWord(msg);
	cerr << result << " " << result.size() << endl;

	  // This writes "0"
	string s = getFirstWord(" $@#%!!");
	cerr << s.size() << endl;
}

void test()
{
    string s = "***AMAZING!*** Do it, now!!";
    string w = extractWord(s);
      // This writes "AMAZING" and "!*** Do it, now!!"
    cerr << w << endl << s << endl;

    w = extractWord(s);
      // This writes "Do" and " it, now!!" (space before "it")
    cerr << w << endl << s << endl;

    w = extractWord(s);
      // This writes "it" and ", now!!"
    cerr << w << endl << s << endl;

    w = extractWord(s);
      // This writes "now" and "!!"
    cerr << w << endl << s << endl;

    w = extractWord(s);
      // This writes "" and "" (both empty strings)
    cerr << w << endl << s << endl;
}

const bool unitTesting = true;

	int main()
	{
	    if (unitTesting)
	    {
	        doUnitsTests();
		return 0;
	    }
	    …  // code for the normal behavior goes here
	}

	void doUnitTests()
		{
		    string s;
		    for (;;)
		    {
		        cerr << "Enter text: ";
			getline(cin, s);
			if (s == "quit")
			    break;
			cerr << "isUppercase returns ";
			if (isUppercase(s))
			    cerr << "true" << endl;
			else
			    cerr << "false" << endl;
			cerr << "getFirstWord returns ";
			cerr << getFirstWord(s) << endl;
		    }
		}

	void doUnitTests()
	{
	    if (getFirstWord("hello there") == "hello")
		cerr << "Passed test 1: getFirstWord(\"hello there\") == \"hello\")" << endl;
	    if (!isUppercase("WoW"))
		cerr << "Passed test 2: !isUppercase(\"WoW\")" << endl;
	    …
	}

#include <cassert>


void doUnitTests()
{
    assert(getFirstWord("hello there") == "hello");
    assert( isUppercase("WOW!!") );
    assert( !isUppercase("WoW!!") );
        string s = "***hello there";
        assert( extractWord(s) == "hello"  &&  s == " there" );
        assert( extractWord(s) == "there"  &&  s == "" );
        assert( extractWord(s) == ""  &&  s == "" );
    …
    cerr << "All tests succeeded" << endl;
}


int main(char** args, int argc)
{




} // end main
