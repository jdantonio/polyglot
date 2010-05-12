package engine
{
	import flexunit.framework.TestCase;
	
	public class GameIntegrationTest extends TestCase
	{
		public function testSimplePvPGame():void
		{
			// create a new game
			var board:Board = new Board();
			
			// create the first player
			var player1:Player = new TestPlayer(board, Player.BLACK);			
			
			// create the second player
			var player2:Player = new TestPlayer(board, Player.RED);			

			// set active player
			var active_player:Player = player1;
			
			// while no winner and no tie
			while (board.winner == Player.NONE && ! board.is_tie())
			{
				// active player moves
				while (board.drop_piece(active_player.color, active_player.make_move()) != Board.INVALID_MOVE) { }
				
				// switch active player
				if (active_player == player1) {
					active_player = player2;
				} else {
					active_player = player1;
				}
			}
			
			// announce the winner
			var winner:int = board.winner;
			assertTrue(board.winner != Player.NONE || board.is_tie());
		}

		public function testSimpleCvCGame():void
		{
			// create a new game
			var board:Board = new Board();
			
			// create the first player
			var player1:Player = new AIPlayer(board, Player.BLACK);			
			
			// create the second player
			var player2:Player = new AIPlayer(board, Player.RED);			
			
			// set active player
			var active_player:Player = player1;
			
			// while no winner and no tie
			while (board.winner == Player.NONE && ! board.is_tie())
			{
				// active player moves
				while (board.drop_piece(active_player.color, active_player.make_move()) != Board.INVALID_MOVE) { }
				
				// switch active player
				if (active_player == player1) {
					active_player = player2;
				} else {
					active_player = player1;
				}
			}
			
			// announce the winner
			var winner:int = board.winner;
			assertTrue(board.winner != Player.NONE || board.is_tie());
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
