package engine
{
	public class Player
	{
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		private var _color:int;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		public function Player(color:int)
		{
			if (GamePieceEnum.is_valid_color(color))
			{
				this._color = color;
			}
			else
			{
				throw TypeError;
			}
		}
	}
}