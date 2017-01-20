package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.DataStreamsDomain;
import com.k2data.app.kmx.enums.Aggregation;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.Order;
import com.k2data.app.kmx.enums.Sign;
import com.k2data.app.kmx.utils.Assert;

import java.util.*;

/**
 * data-streams v0.4.0 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-16.
 */
public class DataStreamsV040Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    private String fieldGroup;
    private String start;
    private String end;
    private Set<String> fields = new HashSet<>();

    private List<String> valueFilters = new ArrayList<>();
    private List<String> valueTrans = new ArrayList<>();

    private String idValue;
    private List<String> orIdValue = new ArrayList<>();
    private List<String> andIdValue = new ArrayList<>();

    private List<String> aggregations = new ArrayList<>();
    private String interval;
    private Boolean naturalTimeBoundary;
    private Object fill;

    private List<String> order = new ArrayList<>();   // 排序
    private Integer size;    // 每页大小 ，注:针对实时接口该字段为每个设备显示的最近点数
    private Integer page;    // 第几页

    private String resultFormatIso;

    public DataStreamsV040Builder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    @Override
    public KmxCond build() {
        Assert.notEmpty(fields, "Fields must not be null");
        Assert.notNull(start, "Start must not be null");
        Assert.notNull(end, "End must not be null");

        String paramsSb = "{" + noSignList(
                objField("fieldGroup", fieldGroup),
                list("fields", new ArrayList<>(fields), true),
                obj("timeRange",
                        objField("start", start),
                        objField("end", end)
                ),
                list("valueFilters", valueFilters),
                list("valueTrans", valueTrans),
                obj("coValueFilter",
                        obj("idFieldFilter",
                                obj(initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)),
                                list("$or", orIdValue),
                                list("$and", andIdValue)
                        )
                ),
                list("aggregations", aggregations),
                obj("aggregationOptions",
                        objField("interval", interval),
                        objField("naturalTimeBoundary", naturalTimeBoundary),
                        objField("fill", fill)
                ),
                list("order", order),
                objField("size", size),
                objField("page", page),
                obj("options", objField("resultTimeFormat", resultFormatIso))
        ) + "}";

        Map<String, String> params = new HashMap<>();
        params.put("query", paramsSb);

        KmxCond cond = new KmxCond();
        cond.setUrl(initParams.getUrls().get(KmxCondType.dataStreams));
        cond.setParams(params);
        cond.setClazz(DataStreamsDomain.class);

        return cond;
    }

    public DataStreamsV040Builder fieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
        return this;
    }

    public DataStreamsV040Builder start(Date start) {
        this.start = start.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataStreamsV040Builder start(String start) {
        this.start = start;
        return this;
    }

    public DataStreamsV040Builder end(Date end) {
        this.end = end.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataStreamsV040Builder end(String end) {
        this.end = end;
        return this;
    }

    /* fields begin */
    public DataStreamsV040Builder fields(Set<String> fields) {
        this.fields = fields;
        return this;
    }

    public DataStreamsV040Builder fields(List<String> fields) {
        this.fields = new HashSet<>(fields);
        return this;
    }

    public DataStreamsV040Builder fields(String[] fields) {
        Collections.addAll(this.fields, fields);
        return this;
    }

    public DataStreamsV040Builder field(String field) {
        this.fields.add(field);
        return this;
    }
    /* fields end */

    public DataStreamsV040Builder idValue(String idValue) {
        this.idValue = idValue;
        return this;
    }

    public DataStreamsV040Builder orIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataStreamsV040Builder andIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataStreamsV040Builder valueFilters(List<String> valueFilters) {
        this.valueFilters = valueFilters;
        return this;
    }

    public DataStreamsV040Builder valueFilters(String valueFilters) {
        this.valueFilters.add(valueFilters);
        return this;
    }

    public DataStreamsV040Builder valueTrans(List<String> valueTrans) {
        this.valueTrans = valueTrans;
        return this;
    }

    public DataStreamsV040Builder valueTrans(String key, String value) {
        this.valueTrans.add("{ \"" + key + "\": " + "\"" + value + "\" }");
        return this;
    }

    public DataStreamsV040Builder valueTrans(String valueTrans) {
        this.valueTrans.add(valueTrans);
        return this;
    }

    public DataStreamsV040Builder aggregations(String aggregations) {
        this.aggregations.add(aggregations);
        return this;
    }

    public DataStreamsV040Builder aggregations(Aggregation... aggregations) {
        this.aggregations.add(buildAggregations("$default", aggregations));
        return this;
    }

    public DataStreamsV040Builder aggregations(String field, Aggregation... aggregations) {
        this.aggregations.add(buildAggregations(field, aggregations));
        return this;
    }

    public DataStreamsV040Builder aggregations(String field, String name, Aggregation aggregation) {
        this.aggregations.add(String.format("{ \"%s\": {\"type\": \"%s\",\"name\": \"%s\"} }",
                field,
                aggregation.getLowerValue(),
                name));
        return this;
    }

    private String buildAggregations(String field, Aggregation... aggregations) {
        StringBuilder sb = new StringBuilder();
        sb.append("{ \"").append(field).append("\": [");
        for (int i = 0; i < aggregations.length; i++) {
            Aggregation aggregation = aggregations[i];

            if (i != 0) {
                sb.append(",");
            }

            sb.append("\"").append(aggregation.getLowerValue()).append("\"");
        }
        sb.append("] }");

        return sb.toString();
    }

    public DataStreamsV040Builder interval(String interval) {
        this.interval = interval;
        return this;
    }

    public DataStreamsV040Builder naturalTimeBoundary(boolean naturalTimeBoundary) {
        this.naturalTimeBoundary = naturalTimeBoundary;
        return this;
    }

    public DataStreamsV040Builder fill(Object fill) {
        this.fill = fill;
        return this;
    }

    public DataStreamsV040Builder order(String field, Order order) {
        this.order.add(String.format("{ \"%s\": \"%s\" }", field, order.getLowerValue()));
        return this;
    }

    public DataStreamsV040Builder order(Order order) {
        this.order.add(String.format("{ \"$default\": \"%s\" }", order.getLowerValue()));
        return this;
    }

    public DataStreamsV040Builder size(Integer size) {
        this.size = size;
        return this;
    }

    public DataStreamsV040Builder page(Integer page) {
        this.page = page;
        return this;
    }

    public DataStreamsV040Builder resultFormatIso() {
        this.resultFormatIso = "iso";
        return this;
    }

}
