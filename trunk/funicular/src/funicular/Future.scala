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
trait Future[+T] extends Function0[T] {
    def forced: Boolean
    def force: T
    def apply = force
    def start: Unit
}

// vim:shiftwidth=4:tabstop=4:expandtab
