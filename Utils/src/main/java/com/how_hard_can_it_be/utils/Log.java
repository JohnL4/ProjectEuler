package com.how_hard_can_it_be.utils;

/**
 * Logging functions.  Currently logs to System.err.
 * @author John Lusk
 *
 */
public class Log
{
    public static void note( String aMsg)
    {
        System.out.println( "NOTE: " + aMsg);
    }
    
    public static void warn( String aMsg)
    {
        System.err.println( "WARNING: " + aMsg);
    }
    
    public static void error( String aMsg)
    {
        System.err.println( "ERROR: " + aMsg);        
    }
}
