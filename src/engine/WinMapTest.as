package engine
{
	import flexunit.framework.TestCase;
	
	public class WinMapTest extends TestCase
	{
		public function testWinIndicies():void
		{
			var map:WinMap = new WinMap(7, 6, 4);

			assertEquals(map.win_indicies(0, 0).join(), "0,24,45");
			assertEquals(map.win_indicies(1, 0).join(), "0,1,27,48");
			assertEquals(map.win_indicies(2, 0).join(), "0,1,2,30,51");
			assertEquals(map.win_indicies(3, 0).join(), "0,1,2,3,33,54,57");
			assertEquals(map.win_indicies(4, 0).join(), "0,1,2,3,36,60");
			assertEquals(map.win_indicies(5, 0).join(), "2,3,39,63");
			assertEquals(map.win_indicies(6, 0).join(), "3,42,66");

			//assertEquals(map.win_indicies(1, 1).join(), "4,5,27,28,45,49");
			//assertEquals(map.win_indicies(2, 2).join(), "8,9,10,31,31,32,45,49,53");
			//assertEquals(map.win_indicies(3, 3).join(), "12,13,14,15,33,34,35,45,49,53");
			//assertEquals(map.win_indicies(2, 4).join(), "16,17,18,31,32,47");
			//assertEquals(map.win_indicies(6, 4).join(), "19,43,44,55");
		}
	}
}

/*******************************************************************************

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
