package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.Attribute;
import com.k2data.app.kmx.domain.Field;
import com.k2data.app.kmx.enums.KmxRestVersion;
import com.k2data.app.kmx.enums.ValueType;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author lidong9144@163.com 2017/5/7
 */
public class FieldGroupsV4BuilderTest {

    private KmxInitParams kmxInitParams = new KmxInitParams(KmxRestVersion.V040);
    private FieldGroupsV4Builder fieldGroupsV4Builder = new FieldGroupsV4Builder(this.kmxInitParams);

    @Test
    public void testPostBuilderFields() {
        FieldGroupsV4Builder.PostBuilder postBuilder = fieldGroupsV4Builder.new PostBuilder();

        Field field = new Field();
        field.setId("fieldId");
        field.setValueType(ValueType.BOOLEAN);

        KmxCond kmxCond = postBuilder.id("id")
                .name("name")
                .addField(field)
                .build();

        String condition = kmxCond.getParams().toString();

        Assert.assertTrue(condition.contains("fields"));
        Assert.assertTrue(condition.contains("fieldId"));
        Assert.assertTrue(!condition.contains("tags"));
        Assert.assertTrue(!condition.contains("attributes"));
    }

    @Test
    public void testPostBuilderTags() {
        FieldGroupsV4Builder.PostBuilder postBuilder = fieldGroupsV4Builder.new PostBuilder();

        Field field = new Field();
        field.setId("fieldId");
        field.setValueType(ValueType.BOOLEAN);

        KmxCond kmxCond = postBuilder.id("id")
                .name("name")
                .addField(field)
                .addTag("tag1", "tag2")
                .build();

        String condition = kmxCond.getParams().toString();

        Assert.assertTrue(condition.contains("tags"));
        Assert.assertTrue(condition.contains("tag1"));
        Assert.assertTrue(condition.contains("tag2"));
        Assert.assertTrue(!condition.contains("attributes"));
    }

    @Test
    public void testPostBuilderAttributes() {
        FieldGroupsV4Builder.PostBuilder postBuilder = fieldGroupsV4Builder.new PostBuilder();

        Field field = new Field();
        field.setId("fieldId");
        field.setValueType(ValueType.BOOLEAN);

        Attribute attribute = new Attribute();
        attribute.setName("attr1");
        attribute.setAttributeValue("attrValue");

        KmxCond kmxCond = postBuilder.id("id")
                .name("name")
                .addField(field)
                .addTag("tag1", "tag2")
                .addAttribute(attribute)
                .build();

        String condition = kmxCond.getParams().toString();

        Assert.assertTrue(condition.contains("tags"));
        Assert.assertTrue(condition.contains("tag1"));
        Assert.assertTrue(condition.contains("tag2"));
        Assert.assertTrue(condition.contains("attributes"));
    }

}
