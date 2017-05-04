package com.k2data.app.kmx.cond;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.Attribute;
import com.k2data.app.kmx.domain.Field;
import com.k2data.app.kmx.domain.FieldGroups;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.RequestType;
import com.k2data.app.kmx.utils.JsonUtils;
import com.k2data.app.kmx.utils.KmxClientUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * fieldsGroup v2 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-19.
 */
public class FieldGroupsV2Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    public FieldGroupsV2Builder(KmxInitParams initParams) {
        this.initParams = initParams;
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

    public AddFieldBuilder addField() {
        return new AddFieldBuilder();
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
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
        public PostBuilder addTag(String... tag) {
            this.tags.addAll(Arrays.asList(tag));
            return this;
        }
        public PostBuilder tags(String... tag) {
            this.tags = Arrays.asList(tag);
            return this;
        }
        public PostBuilder addAttribute(Attribute attribute) {
            this.attributes.add(attribute);
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.fieldGroups));
            kmxCond.setParams(params);
            kmxCond.setClazz(FieldGroups.class);
            kmxCond.setRequestType(RequestType.POST);

            return kmxCond;
        }
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public class GetBuilder {
        private String id;
        private Integer pageSize;
        private Integer page;
        private String order;
        private String aggregation;
        private String select;

        public GetBuilder id(String id) {
            this.id = id;
            return this;
        }
        public GetBuilder pageSize(Integer pageSize) {
            this.pageSize = pageSize;
            return this;
        }
        public GetBuilder page(Integer page) {
            this.page = page;
            return this;
        }
        public GetBuilder order(String order) {
            this.order = order;
            return this;
        }
        public GetBuilder aggregation() {
            this.aggregation = "count";
            return this;
        }
        public GetBuilder select(String... select) {
            this.select = Arrays.stream(select).collect(Collectors.joining(","));
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this));

            String url = initParams.getUrls().get(KmxCondType.fieldGroups);

            KmxCond kmxCond = new KmxCond();
            if (KmxClientUtils.isBlank(id)) {
                kmxCond.setUrl(url);
            } else {
                kmxCond.setUrl(url + '/' + id);
            }
            kmxCond.setParams(params);
            kmxCond.setClazz(FieldGroups.class);
            kmxCond.setRequestType(RequestType.GET);

            return kmxCond;
        }
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public class PutBuilder {
        private String id;
        private List<String> tags;
        private List<Attribute> attributes;

        public PutBuilder id(String id) {
            this.id = id;
            return this;
        }
        public PutBuilder addTag(String... tag) {
            this.tags.addAll(Arrays.asList(tag));
            return this;
        }
        public PutBuilder tags(String... tag) {
            this.tags = Arrays.asList(tag);
            return this;
        }
        public PutBuilder addAttribute(Attribute attribute) {
            this.attributes.add(attribute);
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.fieldGroups) + "/" + id);
            kmxCond.setParams(params);
            kmxCond.setClazz(FieldGroups.class);
            kmxCond.setRequestType(RequestType.PUT);

            return kmxCond;
        }
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    public class AddFieldBuilder {
        private String id;
        private List<Field> fields = new ArrayList<>();

        public AddFieldBuilder id(String id) {
            this.id = id;
            return this;
        }
        public AddFieldBuilder addField(Field field) {
            this.fields.add(field);
            return this;
        }
        public AddFieldBuilder fields(List<Field> fields) {
            this.fields = fields;
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this.fields));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.fieldGroups) + "/addfield/" + id);
            kmxCond.setParams(params);
            kmxCond.setClazz(FieldGroups.class);
            kmxCond.setRequestType(RequestType.PUT);

            return kmxCond;
        }
    }

}
