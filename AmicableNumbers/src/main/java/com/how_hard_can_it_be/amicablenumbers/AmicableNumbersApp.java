package com.how_hard_can_it_be.amicablenumbers;

import java.util.List;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;

import com.how_hard_can_it_be.amicablenumbers.Options;
import com.how_hard_can_it_be.primes.Factor;
import com.how_hard_can_it_be.primes.PrimeUtils;
import com.how_hard_can_it_be.utils.Log;

/**
 * Hello world!
 *
 */
public class AmicableNumbersApp 
{
    public static void main( String[] args )
    {
        Options options = new Options();
        CmdLineParser parser = new CmdLineParser( options);
        boolean goodCommandLine;
        try 
        {
            parser.parseArgument(args);
            goodCommandLine = true;
        }
        catch (CmdLineException exc) 
        {
            goodCommandLine = false;
            System.err.println( exc.getMessage());
            System.err.println("primes [options]");
            parser.printUsage(System.err);
        }
        if (goodCommandLine)
        {
            if (options.ceiling > 0)
                Log.note( String.format( "Will find all amicable numbers less than %d", options.ceiling));
            if (options.numberToFactor > 3)
            {
                
                List<Factor> factors = PrimeUtils.factors(options.numberToFactor);
                System.out.println( String.format( "Found %d factors:", factors.size()));
                for (Factor factor : factors)
                {
                    if (factor.getCount() == 1)
                        System.out.print( String.format( "%8d%12s", factor.getFactor(), ""));
                    else 
                        System.out.print( String.format( "%8d ^ %-8d ", factor.getFactor(), factor.getCount()));
                }
                System.out.println();
            }
            if (options.ceiling > 3)
            {
                List<Factor> factors = Amicable.allFactorsLessThan( options.ceiling);
            }
        }
    }
}
