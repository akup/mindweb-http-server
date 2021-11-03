package net.aklabs.modest;

public class HtmlAttribute {
    public final String key;
    public final String value;
    public final String namespace;

    public HtmlAttribute(String key, String value) {
        this.key = key;
        this.value = value;
        this.namespace = null;
    }
    public HtmlAttribute(String key, String value, String namespace) {
        this.key = key;
        this.value = value;
        this.namespace = namespace;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (this.namespace != null) {
            sb.append(this.namespace);
            sb.append(":");
        }
        sb.append(key);
        sb.append(" = ");
        sb.append(value);
        return sb.toString();
    }
}
