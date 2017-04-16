package com.k2data.app.kmx.cond.serializer;

import java.io.IOException;
import java.lang.reflect.Type;

/**
 * @author lidong 17-1-20.
 */
@FunctionalInterface
public interface BaseSerializer {

    void write(SerializeWriter writer,
               String key,
               Object object, //
               Object fieldName, //
               Type fieldType) throws IOException;

}
