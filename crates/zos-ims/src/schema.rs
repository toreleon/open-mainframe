//! PostgreSQL schema generation for IMS databases.
//!
//! Converts IMS hierarchical database definitions (DBD) to PostgreSQL schemas.
//! Uses a closure table pattern for hierarchical data storage.

use crate::dbd::{DatabaseDefinition, FieldDefinition, FieldType, SegmentDefinition};

/// Generated PostgreSQL schema for an IMS database.
#[derive(Debug, Clone)]
pub struct PostgresSchema {
    /// Database name
    pub name: String,
    /// Generated CREATE TABLE statements
    pub tables: Vec<TableDef>,
    /// Generated CREATE INDEX statements
    pub indexes: Vec<String>,
    /// Generated views for hierarchical navigation
    pub views: Vec<String>,
}

/// Table definition.
#[derive(Debug, Clone)]
pub struct TableDef {
    /// Table name
    pub name: String,
    /// Column definitions
    pub columns: Vec<ColumnDef>,
    /// Primary key columns
    pub primary_key: Vec<String>,
    /// Foreign key constraints
    pub foreign_keys: Vec<ForeignKeyDef>,
}

/// Column definition.
#[derive(Debug, Clone)]
pub struct ColumnDef {
    /// Column name
    pub name: String,
    /// PostgreSQL data type
    pub data_type: String,
    /// Nullable
    pub nullable: bool,
    /// Default value
    pub default: Option<String>,
}

/// Foreign key definition.
#[derive(Debug, Clone)]
pub struct ForeignKeyDef {
    /// Constraint name
    pub name: String,
    /// Local columns
    pub columns: Vec<String>,
    /// Referenced table
    pub ref_table: String,
    /// Referenced columns
    pub ref_columns: Vec<String>,
    /// ON DELETE action
    pub on_delete: String,
}

impl PostgresSchema {
    /// Generate PostgreSQL schema from a DBD.
    pub fn from_dbd(dbd: &DatabaseDefinition) -> Self {
        let mut schema = PostgresSchema {
            name: dbd.name.clone(),
            tables: Vec::new(),
            indexes: Vec::new(),
            views: Vec::new(),
        };

        // Create main segments table
        schema.tables.push(TableDef {
            name: format!("{}_segments", dbd.name.to_lowercase()),
            columns: vec![
                ColumnDef {
                    name: "id".to_string(),
                    data_type: "BIGSERIAL".to_string(),
                    nullable: false,
                    default: None,
                },
                ColumnDef {
                    name: "segment_type".to_string(),
                    data_type: "VARCHAR(8)".to_string(),
                    nullable: false,
                    default: None,
                },
                ColumnDef {
                    name: "parent_id".to_string(),
                    data_type: "BIGINT".to_string(),
                    nullable: true,
                    default: None,
                },
                ColumnDef {
                    name: "sequence_num".to_string(),
                    data_type: "INTEGER".to_string(),
                    nullable: false,
                    default: Some("0".to_string()),
                },
                ColumnDef {
                    name: "data".to_string(),
                    data_type: "JSONB".to_string(),
                    nullable: false,
                    default: Some("'{}'::jsonb".to_string()),
                },
                ColumnDef {
                    name: "raw_data".to_string(),
                    data_type: "BYTEA".to_string(),
                    nullable: true,
                    default: None,
                },
                ColumnDef {
                    name: "created_at".to_string(),
                    data_type: "TIMESTAMPTZ".to_string(),
                    nullable: false,
                    default: Some("CURRENT_TIMESTAMP".to_string()),
                },
                ColumnDef {
                    name: "updated_at".to_string(),
                    data_type: "TIMESTAMPTZ".to_string(),
                    nullable: false,
                    default: Some("CURRENT_TIMESTAMP".to_string()),
                },
            ],
            primary_key: vec!["id".to_string()],
            foreign_keys: vec![ForeignKeyDef {
                name: format!("{}_parent_fk", dbd.name.to_lowercase()),
                columns: vec!["parent_id".to_string()],
                ref_table: format!("{}_segments", dbd.name.to_lowercase()),
                ref_columns: vec!["id".to_string()],
                on_delete: "CASCADE".to_string(),
            }],
        });

        // Create closure table for efficient hierarchical queries
        schema.tables.push(TableDef {
            name: format!("{}_hierarchy", dbd.name.to_lowercase()),
            columns: vec![
                ColumnDef {
                    name: "ancestor_id".to_string(),
                    data_type: "BIGINT".to_string(),
                    nullable: false,
                    default: None,
                },
                ColumnDef {
                    name: "descendant_id".to_string(),
                    data_type: "BIGINT".to_string(),
                    nullable: false,
                    default: None,
                },
                ColumnDef {
                    name: "depth".to_string(),
                    data_type: "INTEGER".to_string(),
                    nullable: false,
                    default: None,
                },
            ],
            primary_key: vec!["ancestor_id".to_string(), "descendant_id".to_string()],
            foreign_keys: vec![
                ForeignKeyDef {
                    name: format!("{}_hierarchy_ancestor_fk", dbd.name.to_lowercase()),
                    columns: vec!["ancestor_id".to_string()],
                    ref_table: format!("{}_segments", dbd.name.to_lowercase()),
                    ref_columns: vec!["id".to_string()],
                    on_delete: "CASCADE".to_string(),
                },
                ForeignKeyDef {
                    name: format!("{}_hierarchy_descendant_fk", dbd.name.to_lowercase()),
                    columns: vec!["descendant_id".to_string()],
                    ref_table: format!("{}_segments", dbd.name.to_lowercase()),
                    ref_columns: vec!["id".to_string()],
                    on_delete: "CASCADE".to_string(),
                },
            ],
        });

        // Create per-segment tables for typed access
        for (seg_name, seg_def) in &dbd.segments {
            schema.tables.push(generate_segment_table(&dbd.name, seg_name, seg_def));
        }

        // Generate indexes
        schema.indexes.push(format!(
            "CREATE INDEX {db}_segments_type_idx ON {db}_segments(segment_type);",
            db = dbd.name.to_lowercase()
        ));
        schema.indexes.push(format!(
            "CREATE INDEX {db}_segments_parent_idx ON {db}_segments(parent_id);",
            db = dbd.name.to_lowercase()
        ));
        schema.indexes.push(format!(
            "CREATE INDEX {db}_segments_data_idx ON {db}_segments USING gin(data);",
            db = dbd.name.to_lowercase()
        ));

        // Generate hierarchical navigation view
        schema.views.push(generate_hierarchy_view(&dbd.name));

        schema
    }

    /// Generate complete SQL script.
    pub fn to_sql(&self) -> String {
        let mut sql = String::new();

        sql.push_str(&format!("-- PostgreSQL schema for IMS database: {}\n", self.name));
        sql.push_str("-- Generated by zos-ims\n\n");

        // Create tables
        for table in &self.tables {
            sql.push_str(&table.to_sql());
            sql.push_str("\n\n");
        }

        // Create indexes
        for index in &self.indexes {
            sql.push_str(index);
            sql.push('\n');
        }
        sql.push('\n');

        // Create views
        for view in &self.views {
            sql.push_str(view);
            sql.push_str("\n\n");
        }

        sql
    }
}

impl TableDef {
    /// Generate CREATE TABLE statement.
    pub fn to_sql(&self) -> String {
        let mut sql = format!("CREATE TABLE {} (\n", self.name);

        // Columns
        let columns: Vec<String> = self
            .columns
            .iter()
            .map(|col| {
                let mut def = format!("    {} {}", col.name, col.data_type);
                if !col.nullable {
                    def.push_str(" NOT NULL");
                }
                if let Some(ref default) = col.default {
                    def.push_str(&format!(" DEFAULT {}", default));
                }
                def
            })
            .collect();

        sql.push_str(&columns.join(",\n"));

        // Primary key
        if !self.primary_key.is_empty() {
            sql.push_str(",\n    PRIMARY KEY (");
            sql.push_str(&self.primary_key.join(", "));
            sql.push(')');
        }

        // Foreign keys
        for fk in &self.foreign_keys {
            sql.push_str(",\n    CONSTRAINT ");
            sql.push_str(&fk.name);
            sql.push_str(" FOREIGN KEY (");
            sql.push_str(&fk.columns.join(", "));
            sql.push_str(") REFERENCES ");
            sql.push_str(&fk.ref_table);
            sql.push('(');
            sql.push_str(&fk.ref_columns.join(", "));
            sql.push_str(") ON DELETE ");
            sql.push_str(&fk.on_delete);
        }

        sql.push_str("\n);");
        sql
    }
}

/// Generate a typed table for a specific segment.
fn generate_segment_table(db_name: &str, seg_name: &str, seg_def: &SegmentDefinition) -> TableDef {
    let table_name = format!(
        "{}_{}_seg",
        db_name.to_lowercase(),
        seg_name.to_lowercase()
    );

    let mut columns = vec![
        ColumnDef {
            name: "segment_id".to_string(),
            data_type: "BIGINT".to_string(),
            nullable: false,
            default: None,
        },
    ];

    // Add columns for each field
    for field in &seg_def.fields {
        columns.push(ColumnDef {
            name: field.name.to_lowercase(),
            data_type: field_to_postgres_type(field),
            nullable: true,
            default: None,
        });
    }

    TableDef {
        name: table_name,
        columns,
        primary_key: vec!["segment_id".to_string()],
        foreign_keys: vec![ForeignKeyDef {
            name: format!("{}_{}_seg_fk", db_name.to_lowercase(), seg_name.to_lowercase()),
            columns: vec!["segment_id".to_string()],
            ref_table: format!("{}_segments", db_name.to_lowercase()),
            ref_columns: vec!["id".to_string()],
            on_delete: "CASCADE".to_string(),
        }],
    }
}

/// Convert IMS field type to PostgreSQL data type.
fn field_to_postgres_type(field: &FieldDefinition) -> String {
    match field.field_type {
        FieldType::Character => format!("VARCHAR({})", field.bytes),
        FieldType::Packed => {
            // Packed decimal: 2 digits per byte + 1 digit + sign
            let digits = field.bytes * 2 - 1;
            format!("NUMERIC({}, 0)", digits)
        }
        FieldType::Zoned => {
            format!("NUMERIC({}, 0)", field.bytes)
        }
        FieldType::Binary => {
            if field.bytes <= 2 {
                "SMALLINT".to_string()
            } else if field.bytes <= 4 {
                "INTEGER".to_string()
            } else {
                "BIGINT".to_string()
            }
        }
        FieldType::Hex => {
            format!("BYTEA") // Hexadecimal data stored as bytea
        }
    }
}

/// Generate hierarchical navigation view.
fn generate_hierarchy_view(db_name: &str) -> String {
    format!(
        r#"CREATE OR REPLACE VIEW {db}_tree AS
WITH RECURSIVE segment_tree AS (
    -- Base case: root segments
    SELECT
        id,
        segment_type,
        parent_id,
        sequence_num,
        data,
        raw_data,
        1 as level,
        ARRAY[id] as path,
        ARRAY[sequence_num] as seq_path
    FROM {db}_segments
    WHERE parent_id IS NULL

    UNION ALL

    -- Recursive case: child segments
    SELECT
        s.id,
        s.segment_type,
        s.parent_id,
        s.sequence_num,
        s.data,
        s.raw_data,
        t.level + 1,
        t.path || s.id,
        t.seq_path || s.sequence_num
    FROM {db}_segments s
    JOIN segment_tree t ON s.parent_id = t.id
)
SELECT * FROM segment_tree
ORDER BY path;"#,
        db = db_name.to_lowercase()
    )
}

/// Generate trigger function for maintaining closure table.
pub fn generate_closure_trigger(db_name: &str) -> String {
    format!(
        r#"-- Trigger function to maintain closure table
CREATE OR REPLACE FUNCTION {db}_maintain_hierarchy()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        -- Insert self-reference
        INSERT INTO {db}_hierarchy (ancestor_id, descendant_id, depth)
        VALUES (NEW.id, NEW.id, 0);

        -- Insert ancestor relationships
        IF NEW.parent_id IS NOT NULL THEN
            INSERT INTO {db}_hierarchy (ancestor_id, descendant_id, depth)
            SELECT ancestor_id, NEW.id, depth + 1
            FROM {db}_hierarchy
            WHERE descendant_id = NEW.parent_id;
        END IF;

        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        -- Cascade delete handled by foreign key
        RETURN OLD;
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER {db}_hierarchy_trigger
AFTER INSERT OR DELETE ON {db}_segments
FOR EACH ROW EXECUTE FUNCTION {db}_maintain_hierarchy();"#,
        db = db_name.to_lowercase()
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dbd::DbdParser;

    const SAMPLE_DBD: &str = r#"
         DBD   NAME=CUSTDB,ACCESS=HIDAM
         SEGM  NAME=CUSTOMER,BYTES=200,PARENT=0
         FIELD NAME=CUSTNO,BYTES=10,START=1,TYPE=C
         FIELD NAME=CUSTNAME,BYTES=50,START=11,TYPE=C
         SEGM  NAME=ORDER,BYTES=100,PARENT=CUSTOMER
         FIELD NAME=ORDNO,BYTES=10,START=1,TYPE=C
         FIELD NAME=ORDAMT,BYTES=8,START=11,TYPE=P
         SEGM  NAME=ITEM,BYTES=80,PARENT=ORDER
         FIELD NAME=ITEMNO,BYTES=10,START=1,TYPE=C
         DBDGEN
    "#;

    #[test]
    fn test_schema_generation() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        let schema = PostgresSchema::from_dbd(&dbd);

        assert_eq!(schema.name, "CUSTDB");
        // Main segments table + hierarchy table + 3 segment tables
        assert_eq!(schema.tables.len(), 5);
    }

    #[test]
    fn test_table_to_sql() {
        let table = TableDef {
            name: "test_table".to_string(),
            columns: vec![
                ColumnDef {
                    name: "id".to_string(),
                    data_type: "BIGSERIAL".to_string(),
                    nullable: false,
                    default: None,
                },
                ColumnDef {
                    name: "name".to_string(),
                    data_type: "VARCHAR(100)".to_string(),
                    nullable: true,
                    default: None,
                },
            ],
            primary_key: vec!["id".to_string()],
            foreign_keys: vec![],
        };

        let sql = table.to_sql();
        assert!(sql.contains("CREATE TABLE test_table"));
        assert!(sql.contains("id BIGSERIAL NOT NULL"));
        assert!(sql.contains("name VARCHAR(100)"));
        assert!(sql.contains("PRIMARY KEY (id)"));
    }

    #[test]
    fn test_field_type_conversion() {
        let char_field = FieldDefinition::new("TEST", 1, 20, FieldType::Character);
        assert_eq!(field_to_postgres_type(&char_field), "VARCHAR(20)");

        let packed_field = FieldDefinition::new("AMT", 1, 8, FieldType::Packed);
        assert_eq!(field_to_postgres_type(&packed_field), "NUMERIC(15, 0)");

        let binary_field = FieldDefinition::new("COUNT", 1, 4, FieldType::Binary);
        assert_eq!(field_to_postgres_type(&binary_field), "INTEGER");
    }

    #[test]
    fn test_full_sql_generation() {
        let mut parser = DbdParser::new();
        let dbd = parser.parse(SAMPLE_DBD).unwrap();

        let schema = PostgresSchema::from_dbd(&dbd);
        let sql = schema.to_sql();

        assert!(sql.contains("CREATE TABLE custdb_segments"));
        assert!(sql.contains("CREATE TABLE custdb_hierarchy"));
        assert!(sql.contains("CREATE TABLE custdb_customer_seg"));
        assert!(sql.contains("CREATE INDEX"));
        assert!(sql.contains("CREATE OR REPLACE VIEW"));
    }
}
