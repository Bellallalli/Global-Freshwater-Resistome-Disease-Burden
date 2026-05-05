import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import shap
import os

# 模型库
from sklearn.linear_model import Ridge, Lasso, BayesianRidge
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.tree import DecisionTreeRegressor
from xgboost import XGBRegressor
from lightgbm import LGBMRegressor

# 工具库
from sklearn.model_selection import KFold, cross_val_predict
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import r2_score, mean_squared_error
from sklearn.pipeline import make_pipeline

# ==========================================
# 1. 数据载入与预处理
# ==========================================
file_path = r"F:\Global Samples\paper data\3-1.social Model\0425-supporting\test_df6-消融risk.xlsx"
df = pd.read_excel(file_path).dropna().reset_index(drop=True)

# 提取信息列、特征(X)和目标(y)
info_columns = df.iloc[:, :9]
X = df.drop(columns=list(df.columns[:9]) + ['DALYs'])
y_true_orig = df['DALYs']
y_log = np.log1p(y_true_orig)

scaler = StandardScaler()
X_scaled = pd.DataFrame(scaler.fit_transform(X), columns=X.columns)

# ==========================================
# 2. 定义模型字典
# ==========================================
models = {
    "DecisionTree": DecisionTreeRegressor(random_state=42),
    "RandomForest": RandomForestRegressor(n_estimators=100, random_state=42),
    "SVR": make_pipeline(StandardScaler(), SVR(C=1.0, epsilon=0.1)),
    "KNN": make_pipeline(StandardScaler(), KNeighborsRegressor(n_neighbors=3)),
    "BayesianRidge": make_pipeline(StandardScaler(), BayesianRidge()),
    "Ridge": make_pipeline(StandardScaler(), Ridge(alpha=1.0)),
    "LASSO": make_pipeline(StandardScaler(), Lasso(alpha=0.1)),
    "LightGBM": LGBMRegressor(n_estimators=100, verbose=-1, random_state=42),
    "XGBoost": XGBRegressor(n_estimators=100, verbosity=0, random_state=42)
}

# ==========================================
# 3. 十折交叉验证与绘图
# ==========================================
kf = KFold(n_splits=10, shuffle=True, random_state=42)
fig, axes = plt.subplots(3, 3, figsize=(18, 15))
axes = axes.flatten()

best_orig_r2 = -np.inf
best_model_name = ""
all_cv_predictions_orig = {}

print("十折交叉验证...")

for i, (name, model) in enumerate(models.items()):
    y_pred_log_cv = cross_val_predict(model, X, y_log, cv=kf)
    y_pred_orig_cv = np.expm1(y_pred_log_cv)
    all_cv_predictions_orig[name] = y_pred_orig_cv

    r2_orig_val = r2_score(y_true_orig, y_pred_orig_cv)
    rmse_orig_val = np.sqrt(mean_squared_error(y_true_orig, y_pred_orig_cv))

    if r2_orig_val > best_orig_r2:
        best_orig_r2 = r2_orig_val
        best_model_name = name

    ax = axes[i]
    ax.scatter(y_log, y_pred_log_cv, alpha=0.5, s=40, color='#5DADE2')
    mn, mx = y_log.min(), y_log.max()
    ax.plot([mn, mx], [mn, mx], 'r--', lw=2)

    raw_ticks = [1, 10, 100]
    tick_locs = [np.log1p(t) for t in raw_ticks]
    raw_labels = [f'$10^{{{int(np.log10(t))}}}$' for t in raw_ticks]
    ax.set_xticks(tick_locs)
    ax.set_xticklabels(raw_labels, fontsize=12)
    ax.set_yticks(tick_locs)
    ax.set_yticklabels(raw_labels, fontsize=12)
    ax.set_xlabel("Observed_DALYs", fontsize=12)
    ax.set_ylabel("Predicted_DALYs", fontsize=12)

    ax.set_title(f"{name}", fontsize=15, fontweight='bold', pad=25)
    ax.text(0.5, 1.03, f"R²: {r2_orig_val:.3f} | RMSE: {rmse_orig_val:.1f}",
            fontsize=13, ha='center', va='bottom', transform=ax.transAxes)
    ax.grid(True, linestyle=':', alpha=0.6)

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.show()

# ==========================================
# 3.5 特征重要性分析 (针对最佳模型)
# ==========================================
print(f"\n正在提取最佳模型 {best_model_name} 的特征重要性...")
best_model_final = models[best_model_name]

# 如果是包含 StandardScaler 的 Pipeline，使用 X_scaled，否则用 X
if hasattr(best_model_final, 'named_steps'):
    best_model_final.fit(X, y_log) # Pipeline 会处理内部 Scaling
else:
    best_model_final.fit(X, y_log)

if best_model_name == "XGBoost":
    importance_types = ['weight', 'gain', 'cover']
    fig_imp, axes_imp = plt.subplots(1, 3, figsize=(22, 7))
    for j, imp_type in enumerate(importance_types):
        scores = best_model_final.get_booster().get_score(importance_type=imp_type)
        imp_df = pd.DataFrame({'Feature': list(scores.keys()), 'Score': list(scores.values())}).sort_values(by='Score', ascending=False)
        sns.barplot(x='Score', y='Feature', data=imp_df, ax=axes_imp[j], palette='viridis')
        axes_imp[j].set_title(f" {imp_type.upper()}", fontsize=14, fontweight='bold')
    plt.suptitle(f"XGBoost Feature Importance Metrics", fontsize=18, fontweight='bold', y=1.02)
    plt.tight_layout(); plt.show()

elif best_model_name == "RandomForest" or best_model_name == "DecisionTree":
    # 针对随机森林和决策树的内置重要性
    importances = best_model_final.feature_importances_
    imp_df = pd.DataFrame({'Feature': X.columns, 'Importance': importances}).sort_values(by='Importance', ascending=False)
    plt.figure(figsize=(10, 6))
    sns.barplot(x='Importance', y='Feature', data=imp_df, palette='magma')
    plt.title(f"Feature Importance: {best_model_name}", fontsize=15, fontweight='bold')
    ax.tick_params(axis='y', labelsize=14)
    plt.grid(True, linestyle='--', alpha=0.5); plt.show()
else:
    print(f"\n当前最佳模型为 {best_model_name}，已跳过算法内置重要性图，将直接进入 SHAP 分析。")

# ==========================================
# 4. SHAP 详细解释
# ==========================================
print(f"\n正在对最佳模型 {best_model_name} 进行增强版 SHAP 分析...")

try:
    # 针对 Pipeline 或非 Pipeline 提取回归器和特征数据
    if hasattr(best_model_final, 'named_steps'):
        regressor_for_shap = best_model_final.named_steps[list(best_model_final.named_steps.keys())[-1]]
        X_input_shap = pd.DataFrame(scaler.transform(X), columns=X.columns)
    else:
        regressor_for_shap = best_model_final
        X_input_shap = X

    if best_model_name in ["RandomForest", "DecisionTree", "XGBoost", "LightGBM"]:
        explainer = shap.TreeExplainer(regressor_for_shap)
        shap_values_obj = explainer(X_input_shap, check_additivity=False)
    else:
        explainer = shap.KernelExplainer(regressor_for_shap.predict, shap.sample(X_input_shap, 50))
        vals = explainer.shap_values(X_input_shap)
        shap_values_obj = shap.Explanation(values=vals, base_values=explainer.expected_value, data=X_input_shap.values, feature_names=X.columns)

    # (1) 蜂群图
    plt.figure(figsize=(10, 8))
    shap.plots.beeswarm(shap_values_obj, max_display=15, show=False)
    plt.title(f"Global Feature Importance (Beeswarm) - {best_model_name}", fontsize=14, pad=20)
    plt.show()

    # (2) 依赖图 (Top 4 特征)
    top_features = X.columns[np.argsort(np.abs(shap_values_obj.values).mean(0))[-4:][::-1]]
    print(f"绘制依赖图: {list(top_features)}")
    fig_dep, axes_dep = plt.subplots(2, 2, figsize=(16, 12))
    axes_dep = axes_dep.flatten()
    for k, col in enumerate(top_features):
        shap.plots.scatter(shap_values_obj[:, col], color=shap_values_obj, ax=axes_dep[k], show=False)
        axes_dep[k].set_title(f"Dependence Plot: {col}", fontsize=12, fontweight='bold')
    plt.tight_layout(); plt.show()

    # (3) 热图
    print("正在绘制 SHAP 热图...")
    plt.figure(figsize=(12, 8))
    sample_size = min(1280, X_input_shap.shape[0])
    shap.plots.heatmap(shap_values_obj[:sample_size], max_display=12, show=False)
    plt.title(f"SHAP Heatmap ({best_model_name})", fontsize=15, pad=30)
    plt.show()

    # (4) SHAP 柱状图（全局重要性）
    print("正在绘制 SHAP 柱状图...")

    plt.figure(figsize=(10, 6))
    shap.plots.bar(shap_values_obj, max_display=15, show=False)
    plt.title(f"Global Feature Importance (Bar) - {best_model_name}", fontsize=14, pad=20)
    plt.show()

    # (5) SHAP 柱状图（高精度版本,手算shap）
    print("正在绘制 SHAP 柱状图（高精度）...")

    # 计算 mean(|SHAP|)
    mean_shap = np.abs(shap_values_obj.values).mean(axis=0)

    imp_df = pd.DataFrame({
        'Feature': X.columns,
        'Mean_SHAP': mean_shap
    }).sort_values(by='Mean_SHAP', ascending=False)

    # 只取前15个
    imp_df = imp_df.head(15)

    plt.figure(figsize=(10, 6))
    bars = plt.barh(imp_df['Feature'], imp_df['Mean_SHAP'])
    plt.gca().invert_yaxis()

    # 在柱子上标注“完整SHAP值”
    for i, v in enumerate(imp_df['Mean_SHAP']):
        plt.text(v, i, f"{v:.5f}", va='center')  # ← 精度自己控制

    plt.xlabel("mean(|SHAP value|)")
    plt.title(f"Global Feature Importance (High Precision) - RandomForest", fontsize=14)
    plt.grid(axis='x', linestyle='--', alpha=0.5)

    plt.show()

except Exception as e:
    print(f"SHAP增强版分析异常: {e}")

# ==========================================
# 5. 保存结果
# ==========================================
save_path = r"F:\Global Samples\paper data\3-1.social Model\0425-supporting\test_df4_res.xlsx"
save_df = info_columns.copy()
save_df['True_DALYs'] = y_true_orig.values
save_df['Predicted_DALYs'] = all_cv_predictions_orig[best_model_name]
save_df.to_excel(save_path, index=False)

print(f"\n任务全部完成！结果已存至: {save_path}")