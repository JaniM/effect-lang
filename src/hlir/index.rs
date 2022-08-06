use std::{collections::HashMap, rc::Rc};

use derive_more::From;
use lasso::Spur;

use super::{
    Builtins, EffectDef, EffectGroup, EffectGroupId, EffectHeader, FnDef, FnHeader, FunctionId,
    Hlir, ModuleId,
};

#[derive(Clone, Debug, From, PartialEq)]
pub enum Header {
    Fn(FnHeader),
    Effect(EffectHeader),
}

#[derive(Clone, Debug, From, PartialEq)]
pub enum Item {
    Fn(FunctionId),
    EffectGroup(EffectGroupId),
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ModuleIndex {
    pub names: HashMap<Spur, Item>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Index {
    pub functions: HashMap<FunctionId, Header>,
    pub effect_groups: HashMap<EffectGroupId, EffectGroup>,
    pub modules: HashMap<ModuleId, ModuleIndex>,
    pub builtins: Rc<Builtins>,
}

impl Index {
    pub fn from_hlir(hlir: &Hlir) -> Self {
        let mut index = Index::default();
        index.builtins = hlir.builtins.clone();

        for module in hlir.modules.values() {
            let mut module_index = ModuleIndex::default();

            for func in module.functions.values() {
                let FnDef {
                    header: header @ FnHeader { id, name, .. },
                    ..
                } = func;

                index.functions.insert(*id, header.clone().into());

                if let Some(name) = name {
                    module_index.names.insert(*name, Item::Fn(*id));
                }
            }

            for group in module.effect_groups.values() {
                module_index
                    .names
                    .insert(group.name, Item::EffectGroup(group.id));

                for effect in &group.effects {
                    let EffectDef {
                        header: header @ EffectHeader { id, name, .. },
                        ..
                    } = effect;

                    index.functions.insert(*id, header.clone().into());
                    module_index.names.insert(*name, Item::Fn(*id));
                }

                index.effect_groups.insert(group.id, group.clone());
            }

            index.modules.insert(module.id, module_index);
        }

        index
    }
}
