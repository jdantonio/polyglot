package engine
{
	import flexunit.framework.TestCase;

	/**
	 * Test the validity of the win indicies stored in a WinMap.
	 * 
	 * The algorithms used to populate the map of win indicies is generic
	 * enough to support any width, height, and number of pieces in a row
	 * required to win, even when the combination makes winning impossible.
	 * This creates several equivalence classes for board configuration
	 * (at least horizontal-only win, vertical-only win, invalid board
	 * dimensions, win impossible, and regular board). A complete test suite
	 * would thoroughly test all of these equivalence classes. When this engine
	 * was first created the engine itself limited itself to only valid boards
	 * and the UI limited the game to a standard 7x6x4 (seven width, six height,
	 * four-in-a-row to win) board. Since the project was created as a learning
	 * exercise a full test suite was not written. This suite tests every
	 * space in a 7x6x4 board and then tests a few other variations. If this
	 * program is ever expanded this test suite should be completed.
	 */
	public class WinMapTest extends TestCase
	{
		/**
		 * Test all indicies in a standard 7-width, 6-height, 4-to-win board.
		 * 
		 * @note The test data provided in the original Pomakis source files is
		 * wrong. The text grid in c4.txt was manually built using a slightly
		 * different set of loop control variables for the diagonal win indicies
		 * than the actual code. The difference is immaterial for the purposes of
		 * understanding how the map works, but it prevents the grid from being
		 * valid test data. The original gris was copied into this source file
		 * because it makes a handy visual reference but the data in this
		 * test case is correct.
		 */
		public function testStandardBoardWinIndicies():void
		{
			var map:WinMap = new WinMap(7, 6, 4);

			assertEquals(map.win_indicies(0, 0).join(), "0,24,45");
			assertEquals(map.win_indicies(1, 0).join(), "0,1,27,46");
			assertEquals(map.win_indicies(2, 0).join(), "0,1,2,30,47");
			assertEquals(map.win_indicies(3, 0).join(), "0,1,2,3,33,48,60");
			assertEquals(map.win_indicies(4, 0).join(), "1,2,3,36,59");
			assertEquals(map.win_indicies(5, 0).join(), "2,3,39,58");
			assertEquals(map.win_indicies(6, 0).join(), "3,42,57");
			
			assertEquals(map.win_indicies(0, 1).join(), "4,24,25,49");
			assertEquals(map.win_indicies(1, 1).join(), "4,5,27,28,45,50");
			assertEquals(map.win_indicies(2, 1).join(), "4,5,6,30,31,46,51,60");
			assertEquals(map.win_indicies(3, 1).join(), "4,5,6,7,33,34,47,52,59,64");
			assertEquals(map.win_indicies(4, 1).join(), "5,6,7,36,37,48,58,63");
			assertEquals(map.win_indicies(5, 1).join(), "6,7,39,40,57,62");
			assertEquals(map.win_indicies(6, 1).join(), "7,42,43,61");

			assertEquals(map.win_indicies(0, 2).join(), "8,24,25,26,53");
			assertEquals(map.win_indicies(1, 2).join(), "8,9,27,28,29,49,54,60");
			assertEquals(map.win_indicies(2, 2).join(), "8,9,10,30,31,32,45,50,55,59,64");
			assertEquals(map.win_indicies(3, 2).join(), "8,9,10,11,33,34,35,46,51,56,58,63,68");
			assertEquals(map.win_indicies(4, 2).join(), "9,10,11,36,37,38,47,52,57,62,67");
			assertEquals(map.win_indicies(5, 2).join(), "10,11,39,40,41,48,61,66");
			assertEquals(map.win_indicies(6, 2).join(), "11,42,43,44,65");
			
			assertEquals(map.win_indicies(0, 3).join(), "12,24,25,26,60");
			assertEquals(map.win_indicies(1, 3).join(), "12,13,27,28,29,53,59,64");
			assertEquals(map.win_indicies(2, 3).join(), "12,13,14,30,31,32,49,54,58,63,68");
			assertEquals(map.win_indicies(3, 3).join(), "12,13,14,15,33,34,35,45,50,55,57,62,67");
			assertEquals(map.win_indicies(4, 3).join(), "13,14,15,36,37,38,46,51,56,61,66");
			assertEquals(map.win_indicies(5, 3).join(), "14,15,39,40,41,47,52,65");
			assertEquals(map.win_indicies(6, 3).join(), "15,42,43,44,48");
			
			assertEquals(map.win_indicies(0, 4).join(), "16,25,26,64");
			assertEquals(map.win_indicies(1, 4).join(), "16,17,28,29,63,68");
			assertEquals(map.win_indicies(2, 4).join(), "16,17,18,31,32,53,62,67");
			assertEquals(map.win_indicies(3, 4).join(), "16,17,18,19,34,35,49,54,61,66");
			assertEquals(map.win_indicies(4, 4).join(), "17,18,19,37,38,50,55,65");
			assertEquals(map.win_indicies(5, 4).join(), "18,19,40,41,51,56");
			assertEquals(map.win_indicies(6, 4).join(), "19,43,44,52");
			
			assertEquals(map.win_indicies(0, 5).join(), "20,26,68");
			assertEquals(map.win_indicies(1, 5).join(), "20,21,29,67");
			assertEquals(map.win_indicies(2, 5).join(), "20,21,22,32,66");
			assertEquals(map.win_indicies(3, 5).join(), "20,21,22,23,35,53,65");
			assertEquals(map.win_indicies(4, 5).join(), "21,22,23,38,54");
			assertEquals(map.win_indicies(5, 5).join(), "22,23,41,55");
			assertEquals(map.win_indicies(6, 5).join(), "23,44,56");
		}
		
		/**
		 * Test a board where it is only possible to win with a horizontal row.
		 */
		public function testHorizontalOnlyWinIndicies():void
		{
			var map:WinMap = new WinMap(7, 3, 6);

			assertEquals(map.win_indicies(0, 0), "0");
			assertEquals(map.win_indicies(1, 0), "0,1");
			assertEquals(map.win_indicies(6, 0), "1");
			assertEquals(map.win_indicies(0, 1), "2");
			assertEquals(map.win_indicies(1, 1), "2,3");
			assertEquals(map.win_indicies(6, 1), "3");
			assertEquals(map.win_indicies(0, 2), "4");
			assertEquals(map.win_indicies(1, 2), "4,5");
			assertEquals(map.win_indicies(6, 2), "5");
		}
		
		/**
		 * Test a board where it is only possible to win with a vertical column.
		 */
		public function testVeticalOnlyWinIndicies():void
		{
			var map:WinMap = new WinMap(3, 7, 6);

			assertEquals(map.win_indicies(0, 0), "0");
			assertEquals(map.win_indicies(0, 1), "0,1");
			assertEquals(map.win_indicies(0, 6), "1");
			assertEquals(map.win_indicies(1, 0), "2");
			assertEquals(map.win_indicies(1, 1), "2,3");
			assertEquals(map.win_indicies(1, 6), "3");
			assertEquals(map.win_indicies(2, 0), "4");
			assertEquals(map.win_indicies(2, 1), "4,5");
			assertEquals(map.win_indicies(2, 6), "5");
		}
		
		/**
		 * Test a board that has invalid dimensions.
		 */
		public function testInvalidBoardWinIndicies():void
		{
			var map:WinMap = new WinMap(-1, -1, 4);
			assertEquals(map.win_indicies(0, 0), "");
		}
		
		/**
		 * Test a board where it is impossible to win.
		 */
		public function testImpossibleWinIndicies():void
		{
			var map:WinMap = new WinMap(2, 2, 4);
			assertEquals(map.win_indicies(0, 0), "");
		}
		
		/**
		 * Test the return value of the win_indicies property when the
		 * board os valid but the requested board location does not exist.
		 */
		public function testInvalidBoardLocation():void
		{
			var map:WinMap = new WinMap(3, 3, 2);
			assertEquals(map.win_indicies(10, 10), "");
		}
	}
}

/*******************************************************************************

 NOTE: The data below has been copied from the original Pomakis source file
 c4.txt. It is slightly incorrect but still useful as an illustration. Please
 see comments to the test case testStandardBoardWinIndicies.

********************************************************************************

sample map[x][y] for x = 7, y = 6, and n = 4:

    +---------+---------+---------+---------+---------+---------+---------+
    |20,26,59 |20,21,29,|20,21,22,|20,21,22,|21,22,23,|22,23,41,|23,44,56 |
    |         |62       |32,65    |23,35,47,|38,50    |53       |         |
  5 |         |         |         |68       |         |         |         |
    |         |         |         |         |         |         |         |
    |         |         |         |         |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+
    |16,25,26,|16,17,28,|16,17,18,|16,17,18,|17,18,19,|18,19,40,|19,43,44,|
    |58       |29,59,61 |31,32,47,|19,34,35,|37,38,49,|41,52,56 |55       |
  4 |         |         |62,64    |46,50,65,|53,68    |         |         |
    |         |         |         |67       |         |         |         |
    |         |         |         |         |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+
    |12,24,25,|12,13,27,|12,13,14,|12,13,14,|13,14,15,|14,15,39,|15,42,43,|
    |26,57    |28,29,47,|30,31,32,|15,33,34,|36,37,38,|40,41,51,|44,54    |
  3 |         |58,60    |46,50,59,|35,45,49,|48,52,56,|55,68    |         |
    |         |         |61,63    |53,62,64,|65,67    |         |         |
    |         |         |         |66       |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+
    |8,24,25, |8,9,27,  |8,9,10,  |8,9,10,  |9,10,11, |10,11,39,|11,42,43,|
    |26,47    |28,29,46,|30,31,32,|11,33,34,|36,37,38,|40,41,54,|44,68    |
  2 |         |50,57    |45,49,53,|35,48,52,|51,55,62,|65,67    |         |
    |         |         |58,60    |56,59,61,|64,66    |         |         |
    |         |         |         |63       |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+
    |4,24,25, |4,5,27,  |4,5,6,30,|4,5,6,7, |5,6,7,36,|6,7,39,  |7,42,43, |
    |46       |28,45,49 |31,48,52,|33,34,51,|37,54,61,|40,64,66 |67       |
  1 |         |         |57       |55,58,60 |63       |         |         |
    |         |         |         |         |         |         |         |
    |         |         |         |         |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+
    |0,24,45  |0,1,27,  |0,1,2,30,|0,1,2,3, |1,2,3,36,|2,3,39,63|3,42,66  |
    |         |48       |51       |33,54,57 |60       |         |         |
  0 |         |         |         |         |         |         |         |
    |         |         |         |         |         |         |         |
    |         |         |         |         |         |         |         |
    +---------+---------+---------+---------+---------+---------+---------+

         0         1         2         3         4         5         6

 0 - 23: horizontal wins
24 - 44: vertical wins
45 - 56: forward diagonal wins
57 - 68: backward diagonal wins

*******************************************************************************/

////////////////////////////////////////////////////////////////////////////////
// 
// Copyright (c) 2010 Jerry D'Antonio <stumpjumper@gmail.com>.
// 
// Licensed under MIT License http://www.opensource.org/licenses/mit-license.php
// 
////////////////////////////////////////////////////////////////////////////////
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
// 
////////////////////////////////////////////////////////////////////////////////
