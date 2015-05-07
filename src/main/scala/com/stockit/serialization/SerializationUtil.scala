package com.stockit.serialization

import java.io._

import com.stockit.base64Coder.Base64Coder
import sun.misc.BASE64Decoder

/**
 * Created by dmcquill on 5/6/15.
 */
class SerializationUtil {
//    private static Object fromString( String s ) throws IOException ,
//    ClassNotFoundException {
//        byte [] data = Base64Coder.decode( s );
//        ObjectInputStream ois = new ObjectInputStream(
//            new ByteArrayInputStream(  data ) );
//        Object o  = ois.readObject();
//        ois.close();
//        return o;
//    }

    @throws[IOException]
    @throws[ClassNotFoundException]
    def objectFromString(string: String): AnyRef = {
        val data: Array[Byte] = Base64Coder.decode(string)
        val objectInputStream = new ObjectInputStream(new ByteArrayInputStream(data))
        val obj = objectInputStream.readObject
        objectInputStream.close
        obj
    }

    /** Write the object to a Base64 string. */
    @throws[IOException]
    def objectToString( obj: AnyRef ): String = {
        val byteArrayOutputStream = new ByteArrayOutputStream
        val outputStream = new ObjectOutputStream(byteArrayOutputStream)
        outputStream.writeObject( obj )
        outputStream.close()
        new String( Base64Coder.encode( byteArrayOutputStream.toByteArray() ) );
    }

}
