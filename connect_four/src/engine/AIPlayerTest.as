package engine
{
	import flexunit.framework.TestCase;
	
	public class AIPlayerTest extends TestCase
	{		
		public function testConstruction():void
		{
			var ai:AIPlayer;
			var color:int = Player.RED;
			var board:Board = new Board();
			
			// test various skill level settings
			
			ai = new AIPlayer(board, color);
			assertEquals(ai.skill, AIPlayer.DEFAULT_SKILL);
			
			ai = new AIPlayer(board, color, AIPlayer.MIN_SKILL - 1);
			assertEquals(ai.skill, AIPlayer.MIN_SKILL);
			
			ai = new AIPlayer(board, color, AIPlayer.MIN_SKILL + 1);
			assertEquals(ai.skill, AIPlayer.MIN_SKILL + 1);
			
			ai = new AIPlayer(board, color, AIPlayer.MAX_SKILL + 1);
			assertEquals(ai.skill, AIPlayer.MAX_SKILL);
		}
		
		public function testAllisOpen():void
		{
			var ai:AIPlayer;
			var board:Board;
			
			// first move of game, odd width (TRUE)
			board = new Board(3, 3, 3);
			ai = new AIPlayer(board, Player.RED);
			assertEquals(ai.make_move(), 1);
			
			// second move of game, odd width (TRUE)
			board = new Board(3, 3, 3);
			board.drop_piece(Player.BLACK, 0);
			ai = new AIPlayer(board, Player.RED);
			assertEquals(ai.make_move(), 1);
		}
		
		public function testDropOrder():void
		{
			assertEquals(AIPlayer.drop_order(3).join(), "1,2,0");
			assertEquals(AIPlayer.drop_order(7).join(), "3,4,2,5,1,6,0");
			assertEquals(AIPlayer.drop_order(8).join(), "3,4,2,5,1,6,0,7");
			assertEquals(AIPlayer.drop_order(10).join(), "4,5,3,6,2,7,1,8,0,9");
		}
	}
}
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
