/*
 *
 * (C) Copyright IBM Corporation 2006-2008.
 *
 *  This file is part of X10 Language.
 *
 */

package funicular

/**
 * The representation of an X10 future expression.
 * @author tardieu
 */
trait Future[+A] extends Function0[A] {
    def forced: Boolean
    def force: A
    def apply = force
    def start: Unit
    def started: Boolean
}

// vim:shiftwidth=4:tabstop=4:expandtab
