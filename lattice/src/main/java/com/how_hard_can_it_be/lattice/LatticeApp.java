package com.how_hard_can_it_be.lattice;

/**
 * Hello world!
 *
 */
public class LatticeApp 
{
    static final int n = 20;
    
    public static void main( String[] args )
    {
        long[][] paths = new long[n+1][n+1];
        
        for (int row = n; row >= 0; row--)
        {
            for (int col = n; col >= 0; col--)
            {
                if (row == n && col == n)
                    // At bottom right corner: no more choices.
                    paths[row][col] = 0;
                else if (row == n || col == n)
                    // Right and bottom edge: only one way to go if you got this far
                    paths[row][col] = 1;
                else
                    paths[row][col] = paths[row][col+1] + paths[row+1][col];
            }
        }
        
        System.out.println( "<html><body>");
        System.out.println( "<table rules='all' border='1' cellpadding='5'>");
        for (int row = 0; row <= n; row++)
        {
            System.out.print( "  <tr>\n    ");
            for (int col=0; col <= n; col++)
                System.out.printf( "<td> %d </td>", paths[row][col]);
            System.out.println( "  </tr>");
        }
        System.out.println( "</table>");
        System.out.println( "</body></html>");
    }
}
