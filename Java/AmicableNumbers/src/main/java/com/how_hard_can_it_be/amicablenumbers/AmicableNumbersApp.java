package com.how_hard_can_it_be.amicablenumbers;

import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.kohsuke.args4j.CmdLineException;
import org.kohsuke.args4j.CmdLineParser;

import com.how_hard_can_it_be.amicablenumbers.AmicableNumbersOptions;
import com.how_hard_can_it_be.primes.Factor;
import com.how_hard_can_it_be.primes.PrimeUtils;
import com.how_hard_can_it_be.utils.Log;

/**
 * Do amicable and perfect numbers stuff.
 *
 */
public class AmicableNumbersApp 
{
    public static void main( String[] args )
    {
        AmicableNumbersOptions options = new AmicableNumbersOptions();
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
            if (options.numberToFactor > 3)
                printPrimeFactors( options.numberToFactor);
            if (options.ceiling > 1)
                findAmicableNumbersLessThan( options.ceiling);
            if (options.numberToType > 1)
                printNumberType( options.numberToType);
            if (options.numberToTypeLow > 1 && options.numberToTypeHigh > options.numberToTypeLow)
            {
                for (int n = options.numberToTypeLow; n <= options.numberToTypeHigh; n++)
                    printNumberType( n);
            }
            if (options.perfectHigh > 1)
                printNumbersLessThanOrEqualTo( options.perfectHigh, NumberType.PERFECT);
            if (options.abundantHigh > 1)
                printNumbersLessThanOrEqualTo( options.abundantHigh, NumberType.ABUNDANT);
            if (options.notSumOfTwoAbundant)
                printNumbersWhichCannotBeWrittenAsSumOfTwoAbundantNumbers();
        }
    }

    private static void findAmicableNumbersLessThan( int aCeiling)
    {
        int[] sumFactors = new int[aCeiling + 1];
        Log.note( String.format( "Will find all amicable numbers less than %d", aCeiling));
        for (int i = 2; i <= aCeiling; i++)
        {
            List<Factor> factors = NumberProperties.properDivisorsOf( i);
            int sum = 0;
            for (Factor factor : factors)
            {
                sum += factor.getFactor();
            }
            sumFactors[i] = sum;
        }
        for (int i = 2; i <= aCeiling; i++)
            for (int j = i + 1; j <= aCeiling; j++)
                if (sumFactors[i] == j && sumFactors[j] == i)
                {
//                    Log.note( String.format( "amicable: %6d and %6d", i, j));
                    System.out.printf( "%d\n%d\n", i, j);
                }
    }

    private static void printPrimeFactors( long aNumberToFactor)
    {
        List<Factor> factors = PrimeUtils.factors(aNumberToFactor);
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

    private static void printNumberType( int aNumberToType)
    {
        EnumSet<NumberType> numberTypes = NumberProperties.numberTypes( aNumberToType);
        System.out.printf( "Types for %d:\n", aNumberToType);
        for (NumberType numberType : numberTypes)
        {
            System.out.printf( "\t%s\n", numberType.toString());
        }
    }
    
    private static void printNumbersLessThanOrEqualTo( int aNumber, NumberType aNumberType)
    {
        for (int n = 2; n <= aNumber; n++)
        {
            EnumSet<NumberType> numTypes = NumberProperties.numberTypes( n);
            if (numTypes.contains( aNumberType))
                System.out.printf(  "%8d", n);
        }
        System.out.println();
    }

    private static void printNumbersWhichCannotBeWrittenAsSumOfTwoAbundantNumbers()
    {
        Set<Integer> abundantSet = new HashSet<Integer>();
        for (int n = 12; n <= 28123; n++ )
            if (NumberProperties.numberTypes( n).contains( NumberType.ABUNDANT))
                abundantSet.add( n);
        
        List<Integer> cannotBeWrittenAsSumOfTwoAbundantNumbers = new LinkedList<Integer>();
        for (int n = 1; n <= 28123; n++)
        {
            boolean canBeWrittenAsSumOfTwoAbundantNumbers = false;
            for (Integer i : abundantSet)
            {
                if (abundantSet.contains( n - i))
                {
                    canBeWrittenAsSumOfTwoAbundantNumbers = true;
                    break;
                }
            }
            if (canBeWrittenAsSumOfTwoAbundantNumbers) {}
            else
                cannotBeWrittenAsSumOfTwoAbundantNumbers.add( n);
        }
        
        int sum = 0;
        for (Integer i : cannotBeWrittenAsSumOfTwoAbundantNumbers)
        {
            sum += i;
            System.out.printf( "%8d", i);
        }
        System.out.printf(  "\nSum = %d\n", sum);
        
    }

}
