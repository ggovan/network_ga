package org.lambdaunbound

package object network_ga {

  lazy val logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

  def ???():Nothing = throw new Exception

  type EdgeMap[N,E] = Map[N,Map[N,E]]

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
