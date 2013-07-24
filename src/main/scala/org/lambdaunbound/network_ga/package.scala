package org.lambdaunbound

package object network_ga {

  def ???():Nothing = throw new Exception

  import java.io.Closeable
  def using[A<:Closeable,B](resource:A)(f:(A=>B)):B = {
    try{
      f(resource)
    }
    finally{
      resource.close()
    }
  }
}