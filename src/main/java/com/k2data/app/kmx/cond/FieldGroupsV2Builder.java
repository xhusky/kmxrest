package com.k2data.app.kmx.cond;

import com.alibaba.fastjson.JSON;
import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.AssetsDomain;
import com.k2data.app.kmx.domain.Attribute;
import com.k2data.app.kmx.domain.Field;
import com.k2data.app.kmx.enums.KmxCondType;
import lombok.Data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * fieldsGroup v2 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-19.
 */
public class FieldGroupsV2Builder extends KmxCondBuilder {

    private KmxInitParams initParams;
    private Map<String, String> params = new HashMap<>();

    public FieldGroupsV2Builder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    @Override
    public KmxCond build() {
        KmxCond kmxCond = new KmxCond();

        return kmxCond;
    }

    public PostBuilder post() {
        return new PostBuilder();
    }

    public GetBuilder get() {
        return new GetBuilder();
    }

    public PutBuilder put() {
        return new PutBuilder();
    }

    @Data
    public class PostBuilder {
        private String id;
        private String name;
        private String description;
        private List<Field> fields = new ArrayList<>();
        private List<String> tags = new ArrayList<>();
        private List<Attribute> attributes = new ArrayList<>();

        public PostBuilder id(String id) {
            this.id = id;
            return this;
        }
        public PostBuilder name(String name) {
            this.name = name;
            return this;
        }
        public PostBuilder description(String description) {
            this.description = description;
            return this;
        }
        public PostBuilder addField(Field field) {
            this.fields.add(field);
            return this;
        }
        public PostBuilder fields(List<Field> fields) {
            this.fields = fields;
            return this;
        }
        public PostBuilder addTag(String tag) {
            this.tags.add(tag);
            return this;
        }
        public PostBuilder addAttribute(Attribute attribute) {
            this.attributes.add(attribute);
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JSON.toJSONString(this));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.fieldGroups));
            kmxCond.setParams(params);
            kmxCond.setClazz(AssetsDomain.class);

            return kmxCond;
        }
    }

    public class GetBuilder {
        /**
         * 添加 get 请求参数
         */
        public GetBuilder addParams(String key, String value) {
            params.put(key, value);
            return this;
        }
    }

    public class PutBuilder {}

}
